use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};
use std::ops::{BitAnd, BitOr, Not};

use crate::{e_and, e_leaf, e_or};
use crate::traits::BuildTruthTable;
use crate::truth_table::TruthTable;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ExpressionNode<T> {
    Leaf(T),
    And(Box<ExpressionNode<T>>, Box<ExpressionNode<T>>),
    Or(Box<ExpressionNode<T>>, Box<ExpressionNode<T>>),
    Not(Box<ExpressionNode<T>>),
}

impl<T> BitAnd for ExpressionNode<T> {
    type Output = ExpressionNode<T>;

    fn bitand(self, rhs: Self) -> Self::Output {
        e_and!(self, rhs)
    }
}

impl<T> BitOr for ExpressionNode<T> {
    type Output = ExpressionNode<T>;

    fn bitor(self, rhs: Self) -> Self::Output {
        e_or!(self, rhs)
    }
}

impl<T: Not<Output=T>> Not for ExpressionNode<T> {
    type Output = ExpressionNode<T>;

    fn not(self) -> Self::Output {
        match self {
            Self::Leaf(filter) => Self::Leaf(filter.not()),
            Self::And(left, right) => e_or!(left.not(), right.not()),
            Self::Or(left, right) => e_and!(left.not(), right.not()),
            Self::Not(parse_tree) => *parse_tree,
        }
    }
}

impl<T: Not<Output=T>> ExpressionNode<T> {
    pub fn to_nnf(self) -> Self {
        match self {
            leaf @ ExpressionNode::Leaf(_) => leaf,
            Self::And(left, right) => e_and!(left.to_nnf(), right.to_nnf()),
            Self::Or(left, right) => e_or!(left.to_nnf(), right.to_nnf()),

            Self::Not(node) => match *node {
                Self::Leaf(filter) => e_leaf!(filter.not()),
                Self::And(left, right) => {
                    e_or!(left.not().to_nnf(), right.not().to_nnf())
                }

                Self::Or(left, right) => {
                    e_and!(left.not().to_nnf(), right.not().to_nnf())
                }
                Self::Not(nested) => nested.to_nnf(),
            },
        }
    }
}

impl<T> ExpressionNode<T> {
    pub fn sort_by_key<K: Ord>(&mut self, mut f: impl FnMut(&T) -> K) {
        self.sort_by_key_internal(&mut f);
    }

    fn sort_by_key_internal<K: Ord>(&mut self, f: &mut impl FnMut(&T) -> K) -> K {
        match self {
            ExpressionNode::Leaf(a) => f(a),
            ExpressionNode::And(left, right) | ExpressionNode::Or(left, right) => {
                let left_value = left.sort_by_key_internal(f);
                let right_value = right.sort_by_key_internal(f);

                if left_value > right_value {
                    std::mem::swap(left, right);
                    left_value
                } else {
                    right_value
                }
            }
            ExpressionNode::Not(node) => {
                node.sort_by_key_internal(f)
            }
        }
    }

    pub fn extract_leafs(&self) -> Vec<&T> {
        let mut result = vec![];
        self.extract_leafs_internal(&mut result);
        result
    }

    fn extract_leafs_internal<'a>(&'a self, result: &mut Vec<&'a T>) {
        match self {
            ExpressionNode::Leaf(value) => {
                result.push(value);
            }
            ExpressionNode::And(left, right) | ExpressionNode::Or(left, right) => {
                left.extract_leafs_internal(result);
                right.extract_leafs_internal(result);
            }
            ExpressionNode::Not(node) => {
                node.extract_leafs_internal(result)
            }
        }
    }
}

impl<T: Ord> ExpressionNode<T> {
    fn evaluate_with(&self, index_map: &BTreeMap<&T, usize>, arrangement: u128) -> bool {
        match self {
            ExpressionNode::Leaf(value) => {
                let index = *index_map.get(value).unwrap();
                arrangement & (1 << index) != 0
            }
            ExpressionNode::And(left, right) => {
                left.evaluate_with(index_map, arrangement) && right.evaluate_with(index_map, arrangement)
            }
            ExpressionNode::Or(left, right) => {
                left.evaluate_with(index_map, arrangement) || right.evaluate_with(index_map, arrangement)
            }
            ExpressionNode::Not(node) => {
                !node.evaluate_with(index_map, arrangement)
            }
        }
    }
}

impl<T: Display> Display for ExpressionNode<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionNode::Leaf(value) => write!(f, "{value}"),
            ExpressionNode::And(left, right) => write!(f, "({left} ∧ {right})"),
            ExpressionNode::Or(left, right) => write!(f, "({left} ∨ {right})"),
            ExpressionNode::Not(node) => write!(f, "¬{node}")
        }
    }
}


impl<'a, T: Ord> BuildTruthTable<'a, T> for ExpressionNode<T> {
    fn build_truth_table(&'a self) -> TruthTable<'a, T> {
        let mut tt = TruthTable::from(self.extract_leafs());

        for arrangement in 0..(1 << tt.num_vars()) {
            let evaluate_result = self.evaluate_with(&tt.var_to_index_map, arrangement);
            tt.add_row(arrangement, evaluate_result);
        }

        tt
    }
}

#[allow(unused_macros)]
pub mod macros {
    #[macro_export]
    macro_rules! e_leaf {
        ($node:expr) => {
            $crate::parse_tree::ExpressionNode::Leaf($node)
        };
    }

    #[macro_export]
    macro_rules! e_not {
        ($node:expr) => {
            $crate::parse_tree::ExpressionNode::Not($node.into())
        };
    }

    #[macro_export]
    macro_rules! e_or {
        ($left:expr, $right:expr) => {
            $crate::parse_tree::ExpressionNode::Or($left.into(), $right.into())
        };
    }

    #[macro_export]
    macro_rules! e_and {
        ($left:expr, $right:expr) => {
            $crate::parse_tree::ExpressionNode::And($left.into(), $right.into())
        };
    }
}

#[cfg(test)]
mod test {
    use crate::{e_and, e_leaf, e_not, e_or};
    use crate::parse_tree::ExpressionNode;
    use crate::traits::BuildTruthTable;

    #[test]
    fn test_to_nnf() {
        assert_eq!((e_leaf!(1) & e_leaf!(2)).to_nnf(), e_leaf!(1) & e_leaf!(2));

        assert_eq!((e_not!(e_leaf!(1))).to_nnf(), e_leaf!(-2));

        assert_eq!(
            e_not!(e_leaf!(true) & e_leaf!(true)).to_nnf(),
            e_or!(e_leaf!(false), e_leaf!(false))
        );

        assert_eq!(
            e_not!(e_leaf!(true) | e_leaf!(true)).to_nnf(),
            e_and!(e_leaf!(false), e_leaf!(false))
        );

        assert_eq!(
            e_not!(e_not!(e_leaf!(true) | e_leaf!(true))).to_nnf(),
            e_or!(e_leaf!(true), e_leaf!(true))
        );
    }

    #[test]
    fn test_not() {
        assert_eq!(
            !e_not!(e_leaf!(true)),
            e_leaf!(true)
        );

        assert_eq!(
            !e_and!(e_leaf!(true), e_leaf!(true)),
            e_or!(e_leaf!(false), e_leaf!(false))
        );

        assert_eq!(
            !e_or!(e_leaf!(true), e_leaf!(true)),
            e_and!(e_leaf!(false), e_leaf!(false))
        );

        assert_eq!(
            !ExpressionNode::Not(e_or!(e_leaf!(true), e_leaf!(true)).into()),
            e_or!(e_leaf!(true), e_leaf!(true))
        );
    }

    #[test]
    fn test_sort() {
        let mut root = e_or!(
            e_not!(
                e_and!(e_leaf!(2000), e_leaf!(1000))
            ),
            e_not!(
                e_or!(
                    e_leaf!(200),
                    e_and!(
                        e_leaf!(5),
                        e_not!(
                            e_and!(
                                e_leaf!(2),
                                e_leaf!(1)
                            )
                        )
                    )
                )
            )
        );

        let expected = e_or!(
            e_not!(
                e_or!(
                    e_and!(
                        e_not!(
                            e_and!(
                                e_leaf!(1),
                                e_leaf!(2)
                            )
                        ),
                        e_leaf!(5)
                    ),
                    e_leaf!(200)
                )
            ),

            e_not!(
                e_and!(e_leaf!(1000), e_leaf!(2000))
            )
        );

        root.sort_by_key(|&leaf_value| leaf_value);

        assert_eq!(root, expected);
    }

    #[test]
    fn test_display() {
        let root = e_not!(
            e_and!(
                e_or!(
                    e_leaf!(1),
                    e_leaf!(2)
                ),
                e_and!(
                    e_leaf!(3),
                    e_leaf!(4)
                )
            )
        );

        assert_eq!(
            format!("{root}"),
            "¬((1 ∨ 2) ∧ (3 ∧ 4))"
        );
    }

    #[test]
    fn test_extract_leafs() {
        let root = e_not!(
            e_and!(
                e_or!(
                    e_leaf!(1),
                    e_leaf!(2)
                ),
                e_and!(
                    e_leaf!(3),
                    e_leaf!(4)
                )
            )
        );

        assert_eq!(
            root.extract_leafs(),
            [&1, &2, &3, &4]
        );
    }

    #[test]
    fn test_build_truth_table() {
        let root = e_and!(
            e_leaf!("a"),
            e_leaf!("b")
        );

        assert_eq!(
            root.build_truth_table().to_matrix(),
            [
                [false, false, false],
                [false, true, false],
                [true, false, false],
                [true, true, true],
            ]
        );

        let root = e_or!(
            e_leaf!("a"),
            e_leaf!("b")
        );

        assert_eq!(
            root.build_truth_table().to_matrix(),
            [
                [false, false, false],
                [false, true, true],
                [true, false, true],
                [true, true, true],
            ]
        );

        let root = e_not!(
            e_or!(
                e_leaf!("a"),
                e_leaf!("b")
            )
        );

        assert_eq!(
            root.build_truth_table().to_matrix(),
            [
                [false, false, true],
                [false, true, false],
                [true, false, false],
                [true, true, false],
            ]
        );

        let root = e_and!(
            e_leaf!("a"),
            e_or!(
                e_leaf!("b"),
                e_leaf!("c")
            )
        );

        assert_eq!(
            root.build_truth_table().to_matrix(),
            [
                [false, false, false, false],
                [false, false, true, false],
                [false, true, false, false],
                [false, true, true, false],
                [true, false, false, false],
                [true, false, true, true],
                [true, true, false, true],
                [true, true, true, true]
            ]
        );
    }
}
