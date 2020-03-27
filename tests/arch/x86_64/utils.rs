
use std::process::Command;
use tempfile::NamedTempFile;
use std::io::Write;

static OBJDUMP_ARGS: &'static str = "objdump -D -Mintel,x86-64 -b binary -m i386 ";

pub fn objdump_all(code: &Vec<u8>) -> String {
    let mut file = NamedTempFile::new().expect("unable to open tmp file");
    file.write_all(code).expect("unable to write");
    let mut command = OBJDUMP_ARGS.to_string();
    command += file.path().to_str().expect("unable to get path");
    command += "| grep \"\\s0:\" -A 256 | cut -d$'\t' -f 3 | sed -E \"s/ +/ /g\" - ";
    String::from_utf8(Command::new("sh")
        .args(&[
            "-c",
            command.as_str(),
        ])
        .output()
        .expect("can't launch sh")
        .stdout)
        .expect("unable to decode output")
}

pub fn objdump(code: &Vec<u8>) -> String {
    let mut file = NamedTempFile::new().expect("unable to open tmp file");
    file.write_all(code).expect("unable to write");
    let mut command = OBJDUMP_ARGS.to_string();
    command += file.path().to_str().expect("unable to get path");
    command += "| grep \"\\s0:\" -m 1 | cut -d$\'\\t\' -f 3 | xargs";
    String::from_utf8(Command::new("sh")
        .args(&[
            "-c",
            command.as_str(),
        ])
        .output()
        .expect("can't launch sh")
        .stdout)
        .expect("unable to decode output")
}