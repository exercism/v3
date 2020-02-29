// TODO define enum variants
pub enum LogLevel {}

pub fn parse_log_level(log: &str) -> LogLevel {
    unimplemented!("parse a log and return the level")
}
pub fn output_for_short_log(kind: LogLevel, log: &str) -> &str {
    unimplemented!("print a shortened log line")
}
