fn main() {
    println!("cargo:rustc-check-cfg=cfg(nightly_diagnostics)");

    #[cfg(feature = "nightly")]
    if let Some((version, channel, _)) = version_check::triple() {
        if version.at_least("1.31.0") && channel.supports_features() {
            println!("cargo:rustc-cfg=nightly_diagnostics");
        }
    }
}
