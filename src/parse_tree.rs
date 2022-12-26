use std::ops::{BitAnd, BitOr, Not};

use crate::{e_and, e_leaf, e_or};

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
}
