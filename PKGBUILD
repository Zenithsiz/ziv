# Maintainer: Filipe Rodrigues <filipejacintorodrigues1@gmail.com>
pkgname=ziv
pkgver=0.1.6
pkgrel=1
pkgdesc="Zenithsiz's image viewer"
arch=('x86_64')
url="https://github.com/zenithsiz/ziv"
license=('MIT' 'APACHE-2')
depends=('gcc-libs')
makedepends=('cargo-nightly')
source=("$pkgname-$pkgver.tar.gz::https://github.com/zenithsiz/$pkgname/archive/$pkgver.tar.gz")
sha512sums=('e55fa7d9b5d3181ff115dabe054c6956d1decbc3125e91e131f7682cce7483dbe2ce9f04d3b39307e3b207e6f6b2d932db3aa9ae28bf8e13508336291f351779')
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
