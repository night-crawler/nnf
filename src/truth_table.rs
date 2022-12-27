use std::collections::{BTreeMap, BTreeSet};

pub struct TruthTable<'a, T> {
    pub var_to_index_map: BTreeMap<&'a T, usize>,
    pub index_to_var_map: BTreeMap<usize, &'a T>,
    rows: BTreeMap<u128, bool>,
}

impl<'a, T, I> From<I> for TruthTable<'a, T> where I: IntoIterator<Item=&'a T>, T: Ord {
    fn from(iter: I) -> Self {
        let unique = iter.into_iter().collect::<BTreeSet<_>>();

        let index_to_var_map: BTreeMap<usize, &T> = unique.iter().copied().enumerate().collect();
        let var_to_index_map: BTreeMap<&T, usize> = unique.iter().enumerate().map(|(index, &var)| (var, index)).collect();

        Self {
            var_to_index_map,
            index_to_var_map,
            rows: BTreeMap::default(),
        }
    }
}

impl<'a, T> TruthTable<'a, T> {
    pub fn num_vars(&self) -> usize {
        self.var_to_index_map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.var_to_index_map.is_empty()
    }

    pub fn add_row(&mut self, arrangement: u128, result: bool) {
        self.rows.insert(arrangement, result);
    }

    pub fn to_matrix(&self) -> Vec<Vec<bool>> {
        let mut rows = vec![];
        for (&arrangement, &value) in self.rows.iter() {
            let mut row = (0..self.var_to_index_map.len()).map(|bit| arrangement & (1 << bit) != 0).collect::<Vec<_>>();
            row.push(value);
            rows.push(row);
        }

        rows.sort_unstable();
        rows
    }
}


impl<'a, T: Ord> TruthTable<'a, T> {
    pub fn to_matrix_vars<'b>(&'b self, vars: impl IntoIterator<Item=&'b T>) -> BTreeSet<Vec<bool>> where 'b: 'a {
        let unique_vars = vars.into_iter().collect::<BTreeSet<_>>();

        let mut rows = BTreeSet::new();
        for (&arrangement, &value) in self.rows.iter() {
            let mut row = unique_vars.iter()
                .map(|var| *self.var_to_index_map.get(var).unwrap())
                .map(|bit| arrangement & (1 << bit) != 0)
                .collect::<Vec<_>>();
            row.push(value);
            rows.insert(row);
        }

        rows
    }

    pub fn extract_intersecting_vars(&self, other: &TruthTable<'a, T>) -> BTreeSet<&'a T> {
        let self_keys = self.var_to_index_map.keys().copied().collect::<BTreeSet<_>>();
        let other_keys = other.var_to_index_map.keys().copied().collect::<BTreeSet<_>>();

        self_keys.intersection(&other_keys).copied().collect()
    }

    pub fn is_identical(&self, other: &TruthTable<'a, T>) -> bool {
        if self.is_empty() || other.is_empty() {
            return false;
        }

        let vars = self.extract_intersecting_vars(other);
        if vars.is_empty() {
            return false;
        }

        let self_tt = self.to_matrix_vars(vars.clone());
        let other_tt = other.to_matrix_vars(vars);

        // There can be other arrangements leading to broader outcomes in one of the truth tables.
        // If there are more vars in another table, we can't be sure they don't affect the final
        // result. That's why we use a subset.
        if self.num_vars() > other.num_vars() {
            other_tt.is_subset(&self_tt)
        } else {
            self_tt.is_subset(&other_tt)
        }
    }
}