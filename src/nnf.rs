use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Debug, Display, Formatter};
use std::ops::{BitAnd, BitOr, Not};

use crate::{and, or};
use crate::traits::BuildTruthTable;
use crate::truth_table::TruthTable;

#[derive(Debug, Clone)]
pub enum Nnf<V> {
    Var(V, bool),
    And(BTreeSet<Nnf<V>>),
    Or(BTreeSet<Nnf<V>>),
}

impl<V: Eq + Ord> Eq for Nnf<V> {}

impl<V: Eq + Ord> PartialEq<Self> for Nnf<V> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Nnf::Var(left_name, left_value), Nnf::Var(right_name, right_value)) => {
                left_name == right_name && left_value == right_value
            }
            (Nnf::And(ch1), Nnf::And(ch2)) => ch1 == ch2,
            (Nnf::Or(ch1), Nnf::Or(ch2)) => ch1 == ch2,
            _ => false,
        }
    }
}

impl<V: Eq + Ord> PartialOrd<Self> for Nnf<V> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Nnf::Var(left_name, left_value), Nnf::Var(right_name, right_value)) => {
                (left_name, left_value).partial_cmp(&(right_name, right_value))
            }
            (Nnf::Var(_, _), Nnf::And(_)) => Some(Ordering::Less),
            (Nnf::And(_), Nnf::Var(_, _)) => Some(Ordering::Greater),

            (Nnf::Var(_, _), Nnf::Or(_)) => Some(Ordering::Less),
            (Nnf::Or(_), Nnf::Var(_, _)) => Some(Ordering::Greater),

            (Nnf::And(_), Nnf::Or(_)) => Some(Ordering::Less),
            (Nnf::Or(_), Nnf::And(_)) => Some(Ordering::Greater),

            (Nnf::And(left_children), Nnf::And(right_children))
            | (Nnf::Or(left_children), Nnf::Or(right_children)) => {
                left_children.iter().rev().partial_cmp(right_children.iter().rev())
            }
        }
    }
}

impl<V: Eq + Ord> Ord for Nnf<V> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<V: Ord> Nnf<V> {
    pub fn or<I: IntoIterator<Item=Nnf<V>>>(iter: I) -> Self {
        Nnf::Or(BTreeSet::from_iter(iter))
    }

    pub fn and<I: IntoIterator<Item=Nnf<V>>>(iter: I) -> Self {
        Nnf::And(BTreeSet::from_iter(iter))
    }

    pub fn is_clause(&self) -> bool {
        match self {
            or @ Nnf::Or(_) => or.is_simple(),
            _ => false,
        }
    }

    pub fn is_simple(&self) -> bool {
        match self {
            Nnf::Var(_, _) => true,
            Nnf::And(children) | Nnf::Or(children) => {
                let mut unique_var_names = BTreeSet::new();
                for child in children {
                    match child {
                        Nnf::Var(name, _) => {
                            if !unique_var_names.insert(name) {
                                return false;
                            }
                        }
                        Nnf::And(_) | Nnf::Or(_) => return false,
                    }
                }

                true
            }
        }
    }

    pub fn is_cnf(&self) -> bool {
        match self {
            Nnf::And(children) => children.iter().all(|child| child.is_clause()),
            _ => false,
        }
    }

    pub fn extract_vars(&self) -> BTreeSet<&V> {
        let mut vars = BTreeSet::default();
        self.extract_vars_internal(&mut vars);
        vars
    }

    fn extract_vars_internal<'a>(&'a self, vars: &mut BTreeSet<&'a V>) {
        match self {
            Nnf::Var(filter, _) => {
                vars.insert(filter);
            }
            Nnf::And(children) | Nnf::Or(children) => {
                children.iter().for_each(|child| child.extract_vars_internal(vars));
            }
        }
    }

    fn evaluate_with(&self, index_map: &BTreeMap<&V, usize>, arrangement: u128) -> bool {
        match self {
            Nnf::Var(filter, value) => {
                let index = *index_map.get(filter).unwrap();
                (arrangement & (1 << index) != 0) == *value
            }
            Nnf::And(children) => {
                children.iter().all(|child| child.evaluate_with(index_map, arrangement))
            }
            Nnf::Or(children) => {
                children.iter().any(|child| child.evaluate_with(index_map, arrangement))
            }
        }
    }
}

impl<V: Ord + Clone> Nnf<V> {
    pub fn has_inversions(&self) -> bool {
        let children = match self {
            Nnf::Var(_, _) => return false,
            Nnf::And(children) | Nnf::Or(children) => children,
        };

        for child in children {
            if children.contains(&!child) {
                return true;
            }
        }

        false
    }
}

impl<V: Ord + PartialOrd + Clone> BitOr for Nnf<V> {
    type Output = Nnf<V>;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // Var | Var
            (left @ Nnf::Var(_, _), right @ Nnf::Var(_, _)) => or!(left, right),

            // Or | Var
            (Nnf::Or(mut children), var @ Nnf::Var(_, _))
            | (var @ Nnf::Var(_, _), Nnf::Or(mut children)) => {
                children.insert(var);
                Nnf::Or(children)
            }

            // optimize single var expressions
            (Nnf::Or(mut left_children), Nnf::And(right_children))
            | (Nnf::And(mut left_children), Nnf::Or(right_children))
            | (Nnf::And(mut left_children), Nnf::And(right_children))
            if left_children.len() == 1 && right_children.len() == 1 => {
                left_children.extend(right_children);
                Nnf::Or(left_children)
            }

            // and(a) | or(b, c) = or(a, b, c)
            (Nnf::And(and_children), Nnf::Or(mut or_children))
            | (Nnf::Or(mut or_children), Nnf::And(and_children))
            if and_children.len() == 1 => {
                or_children.extend(and_children);
                Nnf::Or(or_children)
            }

            // a | and(b)
            (var @ Nnf::Var(_, _), Nnf::And(mut children))
            | (Nnf::And(mut children), var @ Nnf::Var(_, _))
            if children.len() == 1 => {
                children.insert(var);
                Nnf::Or(children)
            }

            // Or | And
            (Nnf::Or(mut children), right @ Nnf::And(_))
            | (right @ Nnf::And(_), Nnf::Or(mut children)) => {
                children.insert(right);
                Nnf::Or(children)
            }
            // Or | Or
            (Nnf::Or(mut left_children), Nnf::Or(right_children)) => {
                left_children.extend(right_children);
                Nnf::Or(left_children)
            }

            // Var | And
            (left @ Nnf::Var(_, _), right @ Nnf::And(_))
            | (left @ Nnf::And(_), right @ Nnf::Var(_, _)) => or!(left, right),

            // And | And
            (left @ Nnf::And(_), right @ Nnf::And(_)) => or!(left, right),
        }
    }
}

impl<V: Ord + PartialOrd + Clone> BitAnd for Nnf<V> {
    type Output = Nnf<V>;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            // Var & Var
            (left @ Nnf::Var(_, _), right @ Nnf::Var(_, _)) => and!(left, right),

            // Var & And
            (var @ Nnf::Var(_, _), Nnf::And(mut children))
            | (Nnf::And(mut children), var @ Nnf::Var(_, _)) => {
                children.insert(var);
                Nnf::And(children)
            }

            // optimize single var expressions
            (Nnf::Or(mut left_children), Nnf::And(right_children))
            | (Nnf::And(mut left_children), Nnf::Or(right_children))
            | (Nnf::Or(mut left_children), Nnf::Or(right_children))
            if left_children.len() == 1 && right_children.len() == 1 => {
                left_children.extend(right_children);
                Nnf::And(left_children)
            }

            // a & or(b)
            (var @ Nnf::Var(_, _), Nnf::Or(mut or_children))
            | (Nnf::Or(mut or_children), var @ Nnf::Var(_, _))
            if or_children.len() == 1 => {
                or_children.insert(var);
                Nnf::And(or_children)
            }

            // and(a, b) & or(c) = and(a, b, c)
            (Nnf::And(mut and_children), Nnf::Or(or_children))
            | (Nnf::Or(or_children), Nnf::And(mut and_children))
            if or_children.len() == 1 => {
                and_children.extend(or_children);
                Nnf::And(and_children)
            }

            // Var & Or
            (var @ Nnf::Var(_, _), or @ Nnf::Or(_))
            | (or @ Nnf::Or(_), var @ Nnf::Var(_, _)) => and!(var, or),

            // And & And
            (Nnf::And(mut left_children), Nnf::And(right_children)) => {
                left_children.extend(right_children);
                Nnf::And(left_children)
            }

            // And & Or
            (Nnf::And(mut and_children), or @ Nnf::Or(_))
            | (or @ Nnf::Or(_), Nnf::And(mut and_children)) => {
                and_children.insert(or);
                Nnf::And(and_children)
            }

            (left @ Nnf::Or(_), right @ Nnf::Or(_)) => and!(left, right),
        }
    }
}

impl<V: Ord + PartialOrd + Clone> Not for Nnf<V> {
    type Output = Nnf<V>;

    fn not(self) -> Self::Output {
        match self {
            Nnf::Var(name, value) => Nnf::Var(name, !value),
            Nnf::And(children) => Nnf::or(children.into_iter().map(|child| !child)),
            Nnf::Or(children) => Nnf::and(children.into_iter().map(|child| !child))
        }
    }
}

impl<V: Ord + PartialOrd + Clone> Not for &Nnf<V> {
    type Output = Nnf<V>;

    fn not(self) -> Self::Output {
        match self {
            Nnf::Var(name, value) => Nnf::Var(name.clone(), !*value),
            Nnf::And(children) => Nnf::or(children.iter().map(|child| !child)),
            Nnf::Or(children) => Nnf::and(children.iter().map(|child| !child))
        }
    }
}

impl<V: Display> Display for Nnf<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Nnf::Var(name, value) => {
                if *value {
                    write!(f, "{name}")
                } else {
                    write!(f, "¬{name}")
                }
            }
            Nnf::And(children) => {
                let parts = children.iter().map(|child| child.to_string()).collect::<Vec<_>>();
                if parts.len() == 1 {
                    write!(f, "{}", parts.join(" ∧ "))
                } else {
                    write!(f, "({})", parts.join(" ∧ "))
                }
            }
            Nnf::Or(children) => {
                let parts = children.iter().map(|child| child.to_string()).collect::<Vec<_>>();
                if parts.len() == 1 {
                    write!(f, "{}", parts.join(" ∨ "))
                } else {
                    write!(f, "({})", parts.join(" ∨ "))
                }
            }
        }
    }
}

impl<'a, T: Ord> BuildTruthTable<'a, T> for Nnf<T> {
    fn build_truth_table(&'a self) -> TruthTable<'a, T> {
        let mut tt = TruthTable::from(self.extract_vars());

        for arrangement in 0..(1 << tt.num_vars()) {
            let evaluate_result = self.evaluate_with(&tt.var_to_index_map, arrangement);
            tt.add_row(arrangement, evaluate_result);
        }

        tt
    }
}

pub mod macros {
    #[macro_export]
    macro_rules! var {
        ($name:literal) => {
            if $name.starts_with("!") {
                $crate::nnf::Nnf::Var(&$name[1..], false)
            } else {
                $crate::nnf::Nnf::Var($name, true)
            }
        };
        ($name:expr, $val:expr) => {
            $crate::nnf::Nnf::Var($name, $val)
        };
        ($name:expr) => {
            $crate::nnf::Nnf::Var($name, true)
        };
    }

    #[macro_export]
    macro_rules! or {
        (
            $($expression:literal),+
        ) => {
            or!(
                $(
                    if $expression.starts_with("!") {
                        var!(&$expression[1..], false)
                    } else {
                        var!($expression)
                    }
                ),+
            )
        };

        (
            $($expression:expr),+
        ) => {
            $crate::nnf::Nnf::Or({
                let mut children = std::collections::BTreeSet::new();
                $(
                    children.insert($expression);
                )+
                children
            })
        };

    }

    #[macro_export]
    macro_rules! and {
        (
            $($expression:literal),+
        ) => {
            and!(
                $(
                    if $expression.starts_with("!") {
                        var!(&$expression[1..], false)
                    } else {
                        var!($expression)
                    }
                ),+
            )
        };

        (
            $($expression:expr),+
        ) => {
            $crate::nnf::Nnf::And({
                let mut children = std::collections::BTreeSet::new();
                $(
                    children.insert($expression);
                )+
                children
            })
        };
    }
}


#[cfg(test)]
mod test {
    use std::collections::BTreeSet;
    use std::ops::Not;

    use crate::*;
    use crate::traits::BuildTruthTable;

    #[test]
    fn test_bit_and_operators() {
        // get rid of single-argument clauses
        assert_eq!(and!("a") & and!("b"), and!("a", "b"));
        assert_eq!(or!("a") & or!("b"), and!("a", "b"));
        assert_eq!(and!("a") & or!("b"), and!("a", "b"));
        assert_eq!(or!("a") & and!("b"), and!("a", "b"));
        // var & or
        assert_eq!(var!("a") & or!("b"), and!("a", "b"));

        assert_eq!(
            var!("a") & or!("b", "c"),
            and!(var!("a"), or!("b", "c"))
        );

        assert_eq!(
            var!("a") & and!("b", "c"),
            and!("a", "b", "c")
        );

        // (a || b) & (c && d) = (c && d && (a || b))
        assert_eq!(
            or!("a", "b") & and!("c", "d"),
            and!(var!("c"), var!("d"), or!("a", "b"))
        );

        assert_eq!(
            or!("a", "b") & var!("c"),
            and!(or!("a", "b"), var!("c"))
        );

        assert_eq!(
            or!("a", "b") & or!("c", "d"),
            and!(or!("a", "b"), or!("c", "d"))
        );

        assert_eq!(
            or!("a") & and!("b", "c"),
            and!("a", "b", "c")
        );

        assert_eq!(
            and!("a", "b") & or!("c", "d"),
            and!(var!("a"), var!("b"), or!("c", "d"))
        );
    }

    #[test]
    fn test_bit_or_operators() {
        // get rid of single-argument clauses
        assert_eq!(and!("a") | and!("b"), or!("a", "b"));
        assert_eq!(or!("a") | or!("b"), or!("a", "b"));
        assert_eq!(and!("a") | or!("b"), or!("a", "b"));
        assert_eq!(or!("a") | and!("b"), or!("a", "b"));
        // var | or
        assert_eq!(var!("a") | or!("b"), or!("a", "b"));

        assert_eq!(
            var!("a") | or!("b", "c"),
            or!("a", "b", "c")
        );

        assert_eq!(
            var!("a") | and!("b", "c"),
            or!(var!("a"), and!("b", "c"))
        );

        // (a || b) || (c && d) = (a || b || (c && d))
        assert_eq!(
            or!("a", "b") | and!("c", "d"),
            or!(var!("a"), var!("b"), and!("c", "d"))
        );

        assert_eq!(
            and!("c", "d") | or!("a", "b"),
            or!(var!("a"), var!("b"), and!("c", "d"))
        );

        assert_eq!(
            or!("a", "b") | var!("c"),
            or!("a", "b", "c")
        );

        assert_eq!(
            or!("a", "b") | or!("c", "d"),
            or!("a", "b", "c", "d")
        );

        assert_eq!(
            and!("a") | or!("b", "c"),
            or!("a", "b", "c")
        );

        assert_eq!(
            var!("a") | and!("b"),
            or!("a", "b")
        );

        assert_eq!(
            and!("a") | var!("b"),
            or!("a", "b")
        );

        assert_eq!(
            and!("a", "b") | and!("c", "d"),
            or!(and!("a", "b"), and!("c", "d"))
        );
    }

    #[test]
    fn test_or_chain() {
        let a = var!("a", true);
        let b = var!("b", false);
        let c = var!("c", true);
        let d = var!("d", false);
        let e = var!("e", true);

        assert_eq!(a.clone() | b.clone(), or!("a", "!b"));

        let or = or!(a.clone(), b.clone());
        assert_eq!(
            or | c.clone() | and!(d.clone()) | or!(e.clone()),
            or!("a", "!b", "c", "!d", "e")
        )
    }

    #[test]
    fn test_and_chain() {
        let a = var!("a", true);
        let b = var!("b", false);
        let c = var!("c", true);
        let d = var!("d", false);
        let e = var!("e", true);

        assert_eq!(a.clone() & b.clone(), and!("a", "!b"));

        let and = and!(a.clone(), b.clone());
        assert_eq!(
            and & c.clone() & or!(d.clone()) & and!(e.clone()),
            and!("a", "!b", "c", "!d", "e")
        )
    }

    #[test]
    fn test_order() {
        assert!(or!("a") < or!("b"));
        assert!(or!("a") > or!("!a"));
        assert!(or!("a", "b") > or!("!a", "b"));

        assert!(var!("a") < and!("a"));

        assert!(or!("a", "b") < or!("!c", "!d"));

        assert!(and!("a", "b") < or!("a", "b"));
    }

    #[test]
    fn test_extract_vars() {
        let sentence = or!(
            var!("a"),
            or!(var!("!b"), and!("!c")),
            and!("a", "d")
        );
        assert_eq!(
            sentence.extract_vars(),
            BTreeSet::from([
                &"a", &"b", &"c", &"d"
            ])
        );
    }

    #[test]
    fn test_is_clause() {
        assert!(!var!("a").is_clause());
        assert!(!and!("a").is_clause());
        assert!(or!("a").is_clause());
    }

    #[test]
    fn test_is_simple() {
        assert!(var!("a").is_simple());
        assert!(and!("a").is_simple());
        assert!(or!("a").is_simple());

        assert!(!or!("a", "!a").is_simple());
        assert!(!and!("a", "!a").is_simple());

        assert!(!and!(var!("a"), and!("b")).is_simple());
    }

    #[test]
    fn test_has_inversions() {
        assert!(and!("a", "!a").has_inversions());
        assert!(!and!("a", "b").has_inversions());
        assert!(!var!("a").has_inversions());
    }

    #[test]
    fn test_bit_not() {
        assert_eq!(var!("a").not(), var!("!a"));
        assert_eq!((&var!("a")).not(), var!("!a"));

        assert_eq!(and!("a", "b", "c").not(), or!("!a", "!b", "!c"));
        assert_eq!((&and!("a", "b", "c")).not(), or!("!a", "!b", "!c"));

        assert_eq!(or!("a", "b", "c").not(), and!("!a", "!b", "!c"));
        assert_eq!((&or!("a", "b", "c")).not(), and!("!a", "!b", "!c"));
    }

    #[test]
    fn test_eq() {
        assert_eq!(var!("a"), var!("a"));
        assert_ne!(var!("a"), var!("!a"));

        assert_ne!(var!("a"), and!("!b"));
        assert_ne!(var!("a"), or!("!b"));
    }

    #[test]
    fn test_truth_table() {
        let sentence = and!(
            or!("!a", "!b"),
            or!("c", "d")
        );

        assert_eq!(
            sentence.build_truth_table().to_matrix(),
            [
                // a      b      c      d      =
                [false, false, false, false, false],
                [false, false, false, true, true],
                [false, false, true, false, true],
                [false, false, true, true, true],
                [false, true, false, false, false],
                [false, true, false, true, true],
                [false, true, true, false, true],
                [false, true, true, true, true],
                [true, false, false, false, false],
                [true, false, false, true, true],
                [true, false, true, false, true],
                [true, false, true, true, true],
                [true, true, false, false, false],
                [true, true, false, true, false],
                [true, true, true, false, false],
                [true, true, true, true, false],
            ]
        );

        for row in sentence.build_truth_table().to_matrix() {
            println!("{:?}", row);
        }
    }
}
