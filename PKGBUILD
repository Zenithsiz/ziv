# Maintainer: Filipe Rodrigues <filipejacintorodrigues1@gmail.com>
pkgname=ziv
pkgver=0.1.8
pkgrel=1
pkgdesc="Zenithsiz's image viewer"
arch=('x86_64')
url="https://github.com/zenithsiz/ziv"
license=('MIT' 'APACHE-2')
depends=('gcc-libs')
makedepends=('cargo-nightly')
source=("$pkgname-$pkgver.tar.gz::https://github.com/zenithsiz/$pkgname/archive/$pkgver.tar.gz")
sha512sums=('f85706b5bf77001443647ae8f4dec0525d587600099ff65adfcda025c90ca47192a256b15773ea7bf2a574722a64ebc0fffa489e159af397420c820225a5649c')
# Note: Required due to a linking error with `zstd`.
options=(!lto)

prepare() {
	cd "$pkgname-$pkgver"

	export RUSTUP_TOOLCHAIN=nightly
	cargo fetch --locked --target "$(rustc -vV | sed -n 's/host: //p')"
}

build() {
	cd "$pkgname-$pkgver"

	export RUSTUP_TOOLCHAIN=nightly
	cargo build --frozen --release
}

package() {
	cd "$pkgname-$pkgver"

	install -Dm0755 -t "$pkgdir/usr/bin/" "target/release/$pkgname"
	install -Dm0644 -t "$pkgdir/usr/share/applications/" "install/ziv.desktop"
}
