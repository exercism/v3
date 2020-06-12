#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Unit {
    Millilitre,
    UsCup,
    UsFluidOnce,
    UsTeaspoon,
    UsTablespoon,
}

pub fn get_volume(volume: &(Unit, f32)) -> f32 {
    volume.1
}

pub fn to_cup(volume: &(Unit, f32)) -> (Unit, f32) {
    match volume {
        (Unit::Millilitre, volume)  => {
            let converted = volume * (1.0/240.0);
            (Unit::UsCup, format!("{:.2}", converted).parse::<f32>().unwrap())
        }
        (_, _) => unimplemented!("Not supported yet")
    }
}
pub fn to_millilitres(volume: &(Unit, f32)) -> (Unit, f32) {
    match volume {
        (Unit::UsCup, volume)  => {
            let converted = volume * 240.0;
            (Unit::Millilitre, format!("{:.2}", converted).parse::<f32>().unwrap())
        }
        (_, _) => unimplemented!("Not supported yet")
    }
}
pub fn convert(volume: &(Unit, f32), destination: Unit) -> (Unit, f32) {
    match (&volume.0, destination) {
        (Unit::UsCup, Unit::UsTablespoon) => {
            (destination, &volume.1 * 13.51)
        }
        (Unit::Millilitre, Unit::UsTablespoon) => {
            (destination, &volume.1 / 15.0)
        }
        (_, _) => unimplemented!("Not supported yet")
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn get_volume_works() {
        assert_eq!(get_volume(&(Unit::UsCup, 2.0)) , 2.0);
    }
    #[test]
    fn to_cup_works() {
        assert_eq!(to_cup(&(Unit::Millilitre, 240.0)), (Unit::UsCup, 1.0));
    }
    #[test]
    fn to_cup_fractional() {
        assert_eq!(to_cup(&(Unit::Millilitre, 840.0)), (Unit::UsCup, 3.5));
    }
    #[test]
    fn to_millilitres_works() {
        assert_eq!(to_millilitres(&(Unit::UsCup, 1.0)), (Unit::Millilitre, 240.0));
    }
    #[test]
    fn convert_cup_to_millilitres() {
        assert_eq!(convert(&(Unit::Millilitre, 15.0), Unit::UsTablespoon), (Unit::UsTablespoon, 1.0));
    }
}
