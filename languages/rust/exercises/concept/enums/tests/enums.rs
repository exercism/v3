use enums::{output_for_short_log, parse_log_level, LogLevel};

#[test]
fn parse_error() {
    assert_eq!(LogLevel::Error, parse_log_level("[ERROR]: Disk full"));
}

#[test]
#[ignore]
fn parse_warning() {
    assert_eq!(
        LogLevel::Warning,
        parse_log_level("[WARNING]: Timezone not set")
    );
}

#[test]
#[ignore]
fn parse_info() {
    assert_eq!(LogLevel::Info, parse_log_level("[INFO]: Timezone changed"));
}

#[test]
#[ignore]
fn parse_unknown() {
    assert_eq!(LogLevel::Unknown("FATAL".into()), parse_log_level("[FATAL]: Crash!"));
}

#[test]
#[ignore]
fn shortened_error() {
    assert_eq!(
        "4:Stack overflow",
        output_for_short_log(LogLevel::Error, "Stack overflow")
    );
}

#[test]
#[ignore]
fn shortened_warning() {
    assert_eq!(
        "2:Unsafe password",
        output_for_short_log(LogLevel::Warning, "Unsafe password")
    );
}

#[test]
#[ignore]
fn shortened_info() {
    assert_eq!(
        "1:File moved",
        output_for_short_log(LogLevel::Info, "File moved")
    );
}

#[test]
#[ignore]
fn shortened_unknown() {
    assert_eq!(
        "0:Something unknown happened",
        output_for_short_log(LogLevel::Unknown, "Something unknown happened")
    );
}
