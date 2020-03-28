use enums::{error, info, log, warn, LogLevel};

#[test]
fn emits_info() {
    assert_eq!(info("Timezone changed"), "[INFO]: Timezone changed");
}

#[test]
#[ignore]
fn emits_warning() {
    assert_eq!(warn("Timezone not set"), "[WARNING]: Timezone not set");
}

#[test]
#[ignore]
fn emits_error() {
    assert_eq!(error("Disk full"), "[ERROR]: Disk full");
}

#[test]
#[ignore]
fn log_emits_info() {
    assert_eq!(
        log(LogLevel::Info, "Timezone changed"),
        "[INFO]: Timezone changed"
    );
}

#[test]
#[ignore]
fn log_emits_warning() {
    assert_eq!(
        log(LogLevel::Warning, "Timezone not set"),
        "[WARNING]: Timezone not set"
    );
}

#[test]
#[ignore]
fn log_emits_error() {
    assert_eq!(log(LogLevel::Error, "Disk full"), "[ERROR]: Disk full");
}
