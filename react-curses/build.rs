use std::env;
use std::process::Command;

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();

    let result = Command::new("deno")
        .args(&[
            "bundle",
            "--config",
            "tsconfig.json",
            "--no-check",
            "app/index.tsx",
        ])
        .arg(&format!("{}/index.bundle.js", out_dir))
        .status()
        .unwrap();

    assert!(result.success(), "bundle javascript");

    //println!("cargo:rerun-if-changed=tsconfig.json");
    //println!("cargo:rerun-if-changed=runtime.d.ts");
    //println!("cargo:rerun-if-changed=app/index.ts");
}
