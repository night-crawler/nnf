use crate::truth_table::TruthTable;

pub trait Render {
    fn render(&self) -> String;
}

pub trait BuildTruthTable<'a, T> {
    fn build_truth_table(&'a self) -> TruthTable<'a, T>;
}

