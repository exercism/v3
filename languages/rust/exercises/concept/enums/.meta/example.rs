#[derive(Clone, PartialEq, Debug)]
pub enum LogLevel {
    Info,
    Warning,
    Error,
    Unknown,
}

pub fn parse_log_level(log: &str) -> LogLevel {
    if log.starts_with("[ERROR]") {
        LogLevel::Error
    } else if log.starts_with("[WARNING]") {
        LogLevel::Warning
    } else if log.starts_with("[INFO]") {
        LogLevel::Info
    } else {
        LogLevel::Unknown
    }
}
pub fn output_for_short_log(kind: LogLevel, log: &str) -> String {
    let code = match kind {
        LogLevel::Error => 4,
        LogLevel::Warning => 2,
        LogLevel::Info => 1,
        LogLevel::Unknown => 0,
    };
    format!("{}:{}", code, log)
}
