# Maintainer: Filipe Rodrigues <filipejacintorodrigues1@gmail.com>
pkgname=ziv
pkgver=0.1.4
pkgrel=1
pkgdesc="Zenithsiz's image viewer"
arch=('x86_64')
url="https://github.com/zenithsiz/ziv"
license=('MIT' 'APACHE-2')
depends=('gcc-libs')
makedepends=('cargo-nightly')
source=("$pkgname-$pkgver.tar.gz::https://github.com/zenithsiz/$pkgname/archive/$pkgver.tar.gz")
sha512sums=('55e643d6f93ab56317b4699953bf8e69f210467cc71b5c47029dcbcffc375222ccca1eea8ea77e5447ec9e6798d983eb8c8b978070da7256851ff0204824300c')
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
