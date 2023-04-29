pub fn is_vec_all_same<T: PartialEq>(v: &[T]) -> bool {
    if let Some(first) = v.first() {
        v.iter().all(|x| x == first)
    } else {
        true
    }
}
