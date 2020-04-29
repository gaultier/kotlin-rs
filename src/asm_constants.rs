use log::debug;
use std::collections::btree_map::Iter;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

type Label = String;

#[derive(Debug)]
pub(crate) struct Constants {
    unique_value_to_label: BTreeMap<String, Label>,
    unique_labels: BTreeSet<String>,
    label_count: usize,
}

impl Constants {
    pub(crate) fn new() -> Constants {
        Constants {
            unique_value_to_label: BTreeMap::new(),
            unique_labels: BTreeSet::new(),
            label_count: 0,
        }
    }

    pub(crate) fn find_or_create_string(&mut self, s: String) -> Label {
        debug!("constants: adding constant: string={}", &s);

        if let Some(label) = self.unique_value_to_label.get(&s) {
            debug!(
                "constants: constant already exists: string={} label={}",
                &s, label
            );
            return label.clone();
        }

        let new_label = self.generate_new_label();
        self.unique_value_to_label.insert(s, new_label.clone());

        new_label
    }

    fn generate_new_label(&mut self) -> Label {
        self.label_count += 1;
        format!("string_{}", self.label_count)
    }

    pub fn iter(&self) -> Iter<'_, String, Label> {
        self.unique_value_to_label.iter()
    }
}

// impl<'a> IntoIterator for &Constants {
//     type Item = (String, Label);
//     type IntoIter = &'a std::collections::btree_map::IntoIter<String, Label>;

//     fn into_iter(self) -> Self::IntoIter {
//         &self.unique_value_to_label.iter()
//     }
// }
