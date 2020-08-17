use std::path::{Path, PathBuf};
use std::process::Command;

const ST_BUILD: u32 = 4076;

const LANGUAGES: [&str; 8] = [
    "sbnf",
    "simplec",
    "regexp",
    "tests/html",
    "tests/issue_6",
    "tests/recursive_branch_points",
    "tests/simple_interpreter",
    "tests/meta_scopes",
];

fn main() -> std::io::Result<()> {
    std::fs::remove_dir_all("target/test/Data/Packages").ok();
    std::fs::create_dir_all("target/test/Data/Packages").unwrap();

    // Download syntax_tests executable
    let tests_exe = format!("target/test/syntax_tests_{}", ST_BUILD);
    let tests_exe_path = Path::new(&tests_exe);

    if !tests_exe_path.exists() {
        let url = format!("https://download.sublimetext.com/st_syntax_tests_build_{}_x64.tar.bz2", ST_BUILD);
        let shell = format!("curl \"{}\" | tar -xjO st_syntax_tests/syntax_tests > {} && chmod u+x {}", url, tests_exe, tests_exe);
        let status = Command::new("sh")
            .arg("-c")
            .arg(&shell)
            .status()
            .expect("Failed to exec shell");
        assert!(status.success(), "Failed to download ST syntax tests executable");
    }

    // Copy all the tests cleanly from the test languages
    for language in &LANGUAGES {
        let mut sbnfs: Vec<PathBuf> = vec!();
        let mut tests: Vec<PathBuf> = vec!();

        let dir = Path::new(language);
        visit_dirs(dir, &mut |entry| {
            if entry.path().file_name().unwrap().to_str().unwrap().starts_with("syntax_test_") {
                tests.push(entry.path());
            } else if entry.path().file_name().unwrap().to_str().unwrap().ends_with(".sbnf") {
                sbnfs.push(entry.path());
            }
        })?;

        if !sbnfs.is_empty() {
            for sbnf in sbnfs {
                let target = PathBuf::from("target/test/Data/Packages").join(sbnf.with_extension("sublime-syntax"));
                std::fs::create_dir_all(target.parent().unwrap()).unwrap();

                // Compile syntax to target
                let status = Command::new("cargo")
                    .args(&["run", "--", sbnf.to_str().unwrap(), "-o", target.to_str().unwrap()])
                    .status()
                    .expect("Failed run exec cargo");
                assert!(status.success(), "Failed to compile {:?}", sbnf);
            }

            // Copy syntax tests
            for test in &tests {
                let dest = PathBuf::from("target/test/Data/Packages").join(test);
                std::fs::create_dir_all(dest.parent().unwrap()).unwrap();

                std::fs::copy(test, dest).unwrap();
            }
        }
    }

    println!("Running {}", tests_exe);
    let status = Command::new(&tests_exe).status()?;
    assert!(status.success(), "Syntax tests failed");

    Ok(())
}

fn visit_dirs(dir: &Path, cb: &mut dyn FnMut(&std::fs::DirEntry)) -> std::io::Result<()> {
    if dir.is_dir() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, cb)?;
            } else {
                cb(&entry);
            }
        }
    }
    Ok(())
}
