use std::collections::BTreeSet;
use std::hint::unreachable_unchecked;

use crate::nnf::Nnf;
use crate::or;

pub struct TseitinTransform<A, V> {
    aux_factory: A,
    clauses: BTreeSet<Nnf<V>>,
}

impl<A, V> TseitinTransform<A, V>
    where
        A: FnMut(&Nnf<V>) -> Nnf<V>,
        V: Ord + Clone,
{
    pub fn new(aux_factory: A) -> Self {
        Self { aux_factory, clauses: BTreeSet::default() }
    }

    fn process_all(&mut self, root: Nnf<V>) {
        match root {
            var @ Nnf::Var(_, _) => {
                self.clauses.insert(or!(var));
            }
            Nnf::And(children) | Nnf::Or(children) if children.len() == 1 => {
                self.process_all(children.into_iter().next().unwrap());
            }
            Nnf::Or(children) => {
                let or = Nnf::or(children.into_iter().map(|child| self.process_node(child)));
                if or.has_inversions() {
                    return;
                }
                self.clauses.insert(or);
            }
            Nnf::And(children) => {
                children.into_iter().for_each(|child| self.process_all(child));
            }
        }
    }

    fn process_node(&mut self, root: Nnf<V>) -> Nnf<V> {
        let processed_node = match root {
            var @ Nnf::Var(_, _) => return var,
            Nnf::And(children) | Nnf::Or(children) if children.len() == 1 => {
                return self.process_node(children.into_iter().next().unwrap());
            }
            Nnf::And(children) => {
                Nnf::and(children.into_iter().map(|child| self.process_node(child)))
            }
            Nnf::Or(children) => {
                Nnf::or(children.into_iter().map(|child| self.process_node(child)))
            }
        };
        let aux = (self.aux_factory)(&processed_node);

        match processed_node {
            Nnf::And(_) if processed_node.has_inversions() => {
                self.clauses.insert(or!(!aux.clone()));
            }

            Nnf::Or(_) if processed_node.has_inversions() => {
                self.clauses.insert(or!(aux.clone()));
            }

            Nnf::And(children) => {
                self.clauses.insert(Nnf::or(
                    children.iter().map(|child| !child).chain(std::iter::once(aux.clone())),
                ));

                for child in children {
                    self.clauses.insert(or!(!aux.clone(), child));
                }
            }

            Nnf::Or(children) => {
                self.clauses
                    .insert(Nnf::or(children.iter().cloned().chain(std::iter::once(!aux.clone()))));

                for child in children {
                    self.clauses.insert(or!(!child, aux.clone()));
                }
            }

            Nnf::Var(_, _) => unsafe { unreachable_unchecked() }
        }

        aux
    }

    pub fn transform(mut self, root: Nnf<V>) -> Nnf<V> {
        self.process_all(root);
        Nnf::And(self.clauses)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use crate::{and, or, var};
    use crate::nnf::Nnf;
    use crate::traits::{BuildTruthTable, Render};
    use crate::tseitin::TseitinTransform;

    fn transform(sentence: Nnf<&'static str>) -> Nnf<&'static str> {
        let mut counter = 0;
        let transformer = TseitinTransform::new(|_| {
            let name: &'static str = Box::leak(Box::new(format!("aux_{}", counter)));
            counter += 1;
            var!(name)
        });

        transformer.transform(sentence)
    }

    #[test]
    fn test_complex() {
        let sentence = or!(
            and!(
                var!("a"),
                and!("b", "c")
            ),
            and!(
                or!("!d", "e"),
                and!("f", "!g")
            )
        );
        assert!(!sentence.render().is_empty());

        assert!(!sentence.is_cnf(), "Expression is not a cnf yet");

        let sentence = transform(sentence);
        assert!(sentence.is_cnf(), "Expression must be in the cnf form after transformation");

        assert_eq!(
            sentence,
            and!(
                or!("a", "!aux_1"),
                or!("aux_0", "!aux_1"),
                or!("aux_1", "aux_4"),
                or!("aux_2", "!aux_4"),
                or!("aux_3", "!aux_4"),
                or!("!aux_0", "b"),
                or!("!aux_0", "c"),
                or!("aux_3", "d"),
                or!("aux_3", "!e"),
                or!("!aux_2", "f"),
                or!("!aux_2", "!g"),
                or!("!a", "!aux_0", "aux_1"),
                or!("!aux_2", "!aux_3", "aux_4"),
                or!("aux_0", "!b", "!c"),
                or!("!aux_3", "!d", "e"),
                or!("aux_2", "!f", "g")
            )
        );

        let transformed_twice = transform(sentence.clone());
        assert!(transformed_twice.is_cnf(), "Double transformation must preserve the cnf form");
        assert_eq!(sentence, transformed_twice);
    }

    #[test]
    fn test_optimizations() {
        assert_eq!(transform(var!("a")), and!(or!("a")));

        assert_eq!(transform(Nnf::<&str>::And(BTreeSet::new())), Nnf::And(BTreeSet::new()));

        assert_eq!(transform(Nnf::<&str>::Or(BTreeSet::new())), and!(Nnf::Or(BTreeSet::new())));

        assert_eq!(transform(or!("a", "b")), and!(or!("a", "b")));

        assert_eq!(
            transform(and!(or!("a", "b"), or!("b", "c"))),
            and!(or!("a", "b"), or!("b", "c"))
        );

        assert_eq!(transform(and!(and!(or!(and!("!a"))))), and!(or!("!a")));
    }

    #[test]
    fn test_single_or_clause() {
        assert_eq!(transform(var!("a") | var!("b")), and!(or!(var!("a"), var!("b"))));

        assert_eq!(
            transform(or!(var!("a"), or!("b", "c"))),
            and!(
                or!("aux_0", "!b"),
                or!("a", "aux_0"),
                or!("aux_0", "!c"),
                or!("c", "b", "!aux_0")
            )
        );
    }

    #[test]
    fn test_or_or_or() {
        let transformed = transform(or!(or!("a", "b"), or!("c", "d")));

        // (aux_0 ∨ ¬a) ∧ (aux_1 ∨ ¬d) ∧ (aux_0 ∨ aux_1) ∧ (¬aux_1 ∨ c ∨ d) ∧ (¬c ∨ aux_1) ∧ (b ∨ ¬aux_0 ∨ a) ∧ (¬b ∨ aux_0)
        assert_eq!(
            transformed,
            and!(
                // (aux_0 ∨ ¬a)
                or!("aux_0", "!a"),
                // (aux_1 ∨ ¬d)
                or!("aux_1", "!d"),
                // (aux_0 ∨ aux_1)
                or!("aux_0", "aux_1"),
                // (¬aux_1 ∨ c ∨ d)
                or!("!aux_1", "c", "d"),
                // (¬c ∨ aux_1)
                or!("!c", "aux_1"),
                // (b ∨ ¬aux_0 ∨ a)
                or!("b", "!aux_0", "a"),
                // (¬b ∨ aux_0)
                or!("!b", "aux_0")
            )
        );
    }

    #[test]
    fn test_and_and_and() {
        let sentence = and!(and!("a", "b"), and!("c", "d"));
        assert_eq!(transform(sentence), and!(or!("a"), or!("b"), or!("c"), or!("d")));
    }

    #[test]
    fn test_and_and_or() {
        let sentence = and!(and!("a", "b"), or!("c", "d"));
        assert_eq!(transform(sentence), and!(or!("a"), or!("b"), or!("c", "d")))
    }

    #[test]
    fn test_single_vars() {
        let sentence = and!(and!("a"), or!("b"));
        assert_eq!(transform(sentence), and!(or!("a"), or!("b")));

        assert_eq!(
            transform(or!(var!("a"), and!("b"))),
            and!(or!("a", "b"))
        );
    }

    #[test]
    fn test_inversions() {
        let sentence = or!(
            var!("a"),
            and!("b", "c"),
            and!("d", "!d")
        );
        assert_eq!(
            transform(sentence),
            and!(
                or!("!aux_1"),
                or!("!aux_0", "b"),
                or!("!aux_0", "c"),
                or!("a", "aux_0", "aux_1"),
                or!("aux_0", "!b", "!c")
            )
        );

        let sentence = or!(
            var!("a"),
            and!("b", "c"),
            or!("d", "!d")
        );
        assert_eq!(
            transform(sentence),
            and!(
                or!("aux_1"),
                or!("!aux_0", "b"),
                or!("!aux_0", "c"),
                or!("a", "aux_0", "aux_1"),
                or!("aux_0", "!b", "!c")
            )
        )
    }

    #[test]
    fn test_transformation_is_identical() {
        let sentence = or!(
            and!(
                var!("a"),
                and!("b", "c")
            ),
            and!(
                or!("!d", "e"),
                and!("f", "!g")
            )
        );
        let original_truth_table = sentence.build_truth_table();

        let transformed_sentence = transform(sentence.clone());
        let transformed_truth_table = transformed_sentence.build_truth_table();

        let vars = original_truth_table.extract_intersecting_vars(&transformed_truth_table);
        assert_eq!(
            vars,
            BTreeSet::from([&"a", &"b", &"c", &"d", &"e", &"f", &"g"])
        );

        assert!(original_truth_table.is_identical(&transformed_truth_table));
    }
}
