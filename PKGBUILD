# Maintainer: Filipe Rodrigues <filipejacintorodrigues1@gmail.com>
pkgname=ziv
pkgver=0.1.2
pkgrel=1
pkgdesc="Zenithsiz's image viewer"
arch=('x86_64')
url="https://github.com/zenithsiz/ziv"
license=('MIT' 'APACHE-2')
depends=('gcc-libs')
makedepends=('cargo-nightly')
source=("$pkgname-$pkgver.tar.gz::https://github.com/zenithsiz/$pkgname/archive/$pkgver.tar.gz")
sha512sums=('5bee6249a7d94da1a471d8ebe72ef923ec1e165e922cde39291d8851ff27d78d3e471ef0924ce8c6409727c8467d1764f298462419d3eae0422049ccc831574b')
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
