# Maintainer: Filipe Rodrigues <filipejacintorodrigues1@gmail.com>
pkgname=ziv
pkgver=0.1.7
pkgrel=1
pkgdesc="Zenithsiz's image viewer"
arch=('x86_64')
url="https://github.com/zenithsiz/ziv"
license=('MIT' 'APACHE-2')
depends=('gcc-libs')
makedepends=('cargo-nightly')
source=("$pkgname-$pkgver.tar.gz::https://github.com/zenithsiz/$pkgname/archive/$pkgver.tar.gz")
sha512sums=('8b97443fb502c147248b35d55c5b10770591a9cc8950eb17537c10183a5b0c6dfdc9cc94929d0cb936a94baa14d2480f902330920adae777d73b8aa2891d4718')
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
