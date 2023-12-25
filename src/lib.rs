pub fn add(left: usize, right: usize) -> usize {
    left + right
}

use std::cmp::Ordering;
use std::fmt::Display;
use std::ops::{Add, Bound};

#[macro_export]
macro_rules! inc {
    [$n:expr] => { ComparableBound::new(Bound::Included($n)) };
}

#[macro_export]
macro_rules! exc {
    [$n:expr] => { ComparableBound::new(Bound::Excluded($n))};
}

#[macro_export]
macro_rules! unb {
    [] => { ComparableBound::new(Bound::Unbounded)};
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ComparableBound<T>
where
    T: PartialOrd,
{
    point: Bound<T>,
}

impl<T> ComparableBound<T>
where
    T: PartialOrd,
{
    #[allow(unused)]
    pub fn new(point: Bound<T>) -> Self {
        Self { point }
    }
}

impl<T> PartialOrd for ComparableBound<T>
where
    T: PartialOrd,
{
    fn ge(&self, other: &Self) -> bool {
        matches!(
            self.partial_cmp(other),
            Some(Ordering::Greater | Ordering::Equal)
        )
    }

    fn gt(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(Ordering::Greater))
    }

    fn le(&self, other: &Self) -> bool {
        matches!(
            self.partial_cmp(other),
            Some(Ordering::Less | Ordering::Equal)
        )
    }

    fn lt(&self, other: &Self) -> bool {
        matches!(self.partial_cmp(other), Some(Ordering::Less))
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (&self.point, &other.point) {
            (Bound::Included(start), Bound::Excluded(end)) => {
                if start > end {
                    Some(Ordering::Greater)
                } else if start == end {
                    Some(Ordering::Greater)
                } else {
                    Some(Ordering::Less)
                }
            }
            (Bound::Excluded(start), Bound::Included(end)) => {
                if start < end {
                    Some(Ordering::Less)
                } else if start == end {
                    Some(Ordering::Less)
                } else {
                    Some(Ordering::Greater)
                }
            }
            (Bound::Included(start), Bound::Included(end)) => start.partial_cmp(&end),
            (Bound::Excluded(start), Bound::Excluded(end)) => start.partial_cmp(&end),
            (Bound::Included(_), Bound::Unbounded) => Some(std::cmp::Ordering::Less),
            (Bound::Excluded(_), Bound::Unbounded) => Some(std::cmp::Ordering::Less),
            (Bound::Unbounded, Bound::Included(_)) => Some(std::cmp::Ordering::Greater),
            (Bound::Unbounded, Bound::Excluded(_)) => Some(std::cmp::Ordering::Greater),
            (Bound::Unbounded, Bound::Unbounded) => None,
        }
    }
}

impl<T> Add<i32> for ComparableBound<T>
where
    T: PartialOrd + Add<i32, Output = T>,
{
    type Output = ComparableBound<T>;

    fn add(self, rhs: i32) -> Self::Output {
        match self.point {
            Bound::Included(p) => ComparableBound::new(Bound::Included(p + rhs)),
            Bound::Excluded(p) => ComparableBound::new(Bound::Excluded(p + rhs)),
            Bound::Unbounded => ComparableBound::new(Bound::Unbounded),
        }
    }
}

impl<I> Display for ComparableBound<I>
where
    I: PartialOrd + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Bound::Included(p) = &self.point {
            f.write_fmt(format_args!("{}", p))
        } else if let Bound::Excluded(p) = &self.point {
            f.write_fmt(format_args!("{}", p))
        } else {
            f.write_fmt(format_args!("Unbounded"))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ComparableBound;
    use std::ops::Bound;

    // Less
    #[test]
    fn less_inc_inc() {
        let a = ComparableBound::new(Bound::Included(5));
        let b = ComparableBound::new(Bound::Included(6));

        assert_eq!(a < b, true);
        assert_eq!(a > b, false);
        assert_eq!(a == b, false);
    }

    #[test]
    fn less_inc_ex() {
        let a = ComparableBound::new(Bound::Included(5));
        let b = ComparableBound::new(Bound::Excluded(6));

        assert_eq!(a < b, true);
        assert_eq!(a > b, false);
        assert_eq!(a == b, false);
    }

    #[test]
    fn less_ex_inc() {
        let a = ComparableBound::new(Bound::Excluded(5));
        let b = ComparableBound::new(Bound::Included(6));

        assert_eq!(a < b, true);
        assert_eq!(a > b, false);
        assert_eq!(a == b, false);
    }

    #[test]
    fn less_ex_ex() {
        let a = ComparableBound::new(Bound::Excluded(5));
        let b = ComparableBound::new(Bound::Excluded(6));

        assert_eq!(a < b, true);
        assert_eq!(a > b, false);
        assert_eq!(a == b, false);
    }

    // Equal
    #[test]
    fn equal_inc_inc() {
        let a = ComparableBound::new(Bound::Included(5));
        let b = ComparableBound::new(Bound::Included(5));

        assert_eq!(a < b, false);
        assert_eq!(a > b, false);
        assert_eq!(a == b, true);
    }

    #[test]
    fn equal_inc_ex() {
        let a = ComparableBound::new(Bound::Included(5));
        let b = ComparableBound::new(Bound::Excluded(5));

        assert_eq!(a < b, false);
        assert_eq!(a > b, true);
        assert_eq!(a == b, false);
    }

    #[test]
    fn equal_ex_inc() {
        let a = ComparableBound::new(Bound::Excluded(5));
        let b = ComparableBound::new(Bound::Included(5));

        assert_eq!(a < b, true);
        assert_eq!(a > b, false);
        assert_eq!(a == b, false);
    }

    #[test]
    fn equal_ex_ex() {
        let a = ComparableBound::new(Bound::Excluded(5));
        let b = ComparableBound::new(Bound::Excluded(5));

        assert_eq!(a < b, false);
        assert_eq!(a > b, false);
        assert_eq!(a == b, true);
    }

    // Greater
    #[test]
    fn greater_inc_inc() {
        let a = ComparableBound::new(Bound::Included(6));
        let b = ComparableBound::new(Bound::Included(5));

        assert_eq!(a < b, false);
        assert_eq!(a > b, true);
        assert_eq!(a == b, false);
    }

    #[test]
    fn greater_inc_ex() {
        let a = ComparableBound::new(Bound::Included(6));
        let b = ComparableBound::new(Bound::Excluded(5));

        assert_eq!(a < b, false);
        assert_eq!(a > b, true);
        assert_eq!(a == b, false);
    }

    #[test]
    fn greater_ex_inc() {
        let a = ComparableBound::new(Bound::Excluded(6));
        let b = ComparableBound::new(Bound::Included(5));

        assert_eq!(a < b, false);
        assert_eq!(a > b, true);
        assert_eq!(a == b, false);
    }

    #[test]
    fn greater_ex_ex() {
        let a = ComparableBound::new(Bound::Excluded(6));
        let b = ComparableBound::new(Bound::Excluded(5));

        assert_eq!(a < b, false);
        assert_eq!(a > b, true);
        assert_eq!(a == b, false);
    }

    #[test]
    fn macros() {
        let a = ComparableBound::new(Bound::Included(0));
        let b = ComparableBound::new(Bound::Excluded(0));
        let c = ComparableBound::new(Bound::Unbounded::<i32>);

        assert_eq!(a, inc![0]);
        assert_eq!(b, exc!(0));
        assert_eq!(c, unb!());
    }
}
