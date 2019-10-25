# Cargo and related

`Cargo.lock` ensures reproducible builds by recording versions of the dependencies when first building, instead of resolving dependencies whenever building. `cargo update` bypasses this.
