pub enum Unit {
    Millilitres,
    UsCup,
    UsFluidOnce,
    UsTeaspoon,
    UsTablespoon,
}   

pub fn get_volume(volume: &(Unit, f32)) -> f32 {
    volume.1
}

pub fn to_cup(volume: &(Unit, f32)) -> (Unit, f32) {
    unimplemented!("Solve me!")
}
pub fn to_millilitres(volume: &(Unit, f32)) -> (Unit, f32) {
    unimplemented!("Solve me!")
}
pub fn convert(volume: &(Unit, f32), destination: Unit) -> (Unit, f32) {
    unimplemented!("Solve me!")
}
