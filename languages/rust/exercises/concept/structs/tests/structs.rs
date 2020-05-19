use structs::*;

const NAME: &str = "Ebenezer";
const AGE: u32 = 89;
const WEIGHT: f32 = 131.6;

#[test]
fn test_get_name() {
    let new_user = User::new(NAME.to_string(), AGE, WEIGHT);
    assert_eq!(new_user.get_name(), NAME);
}

#[test]
#[ignore]
fn test_get_age() {
    let new_user = User::new(NAME.to_string(), AGE, WEIGHT);
    assert_eq!(new_user.get_age(), AGE);
}

#[test]
#[ignore]
fn test_get_weight() {
    let new_user = User::new(NAME.to_string(), AGE, WEIGHT);
    assert_eq!(new_user.get_weight(), WEIGHT);
}

#[test]
#[ignore]
fn test_update_name() {
    let new_name = "Scrooge";
    let mut new_user = User::new(NAME.to_string(), AGE, WEIGHT);
    new_user.update_name(String::from(new_name));
    assert_eq!(new_user.get_name(), new_name);
}

#[test]
#[ignore]
fn test_update_age() {
    let new_age: u32 = 90;
    let mut new_user = User::new(NAME.to_string(), AGE, WEIGHT);
    new_user.update_age(new_age);
    assert_eq!(new_user.get_age(), new_age);
}

#[test]
#[ignore]
fn test_update_weight() {
    let new_weight: f32 = 129.4;
    let mut new_user = User::new(NAME.to_string(), AGE, WEIGHT);
    new_user.update_weight(new_weight);
    assert_eq!(new_user.get_weight(), new_weight);
}
