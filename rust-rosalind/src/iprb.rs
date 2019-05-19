pub fn iprb(homo_d: u32, hetero_d: u32, homo_r: u32) -> f64 {
    let homo_d_f = homo_d as f64;
    let hetero_d_f = hetero_d as f64;
    let homo_r_f = homo_r as f64;

    let total = homo_d_f + hetero_d_f + homo_r_f;
    let mut dom_pairs = homo_d_f * (homo_d_f - 1.0);
    dom_pairs += homo_d_f * hetero_d_f;
    dom_pairs += homo_d_f * homo_r_f;
    dom_pairs += hetero_d_f * homo_d_f;
    dom_pairs += 0.75 * hetero_d_f * (hetero_d_f - 1.0);
    dom_pairs += homo_r_f * homo_d_f;
    dom_pairs += 0.50 * hetero_d_f * homo_r_f;
    dom_pairs += 0.50 * homo_r_f * hetero_d_f;
    dom_pairs / (total * (total - 1.0))
}