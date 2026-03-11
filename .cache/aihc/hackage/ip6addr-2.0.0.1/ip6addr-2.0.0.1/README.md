# ip6addr ![CI](https://github.com/MichelBoucey/ip6addr/actions/workflows/haskell-ci.yml/badge.svg) [![Hackage](https://img.shields.io/hackage/v/ip6addr.svg)](https://hackage.haskell.org/package/ip6addr)

## 1. Purpose

`ip6addr` is a commandline tool that validates and generates IPv6 address text representations, based upon [the library IPv6Addr](https://github.com/MichelBoucey/IPv6Addr) :

* Canonical, in conformation with [RFC 5952](https://www.rfc-editor.org/rfc/rfc5952) (default output)
* Pure, *i.e.* force the rewriting of IPv4 address if present
* Full length
* Reverse lookup domain name, in conformation with [RFC 3596 Section 2.5](https://www.rfc-editor.org/rfc/rfc3596#section-2.5)
* Windows UNC path name
* Random generation for test purpose  

## 2. Install

### 2.1. With Cabal

```
cabal install ip6addr
```

### 2.2. With Nix

#### 2.2.1. From sources

```
nix-build
```

#### 2.2.2. From Nix Packages collection

```
nix profile install nixpkgs#haskellPackages.ip6addr
```

## 3. Usage

```
ip6addr v2.0.0 (c) Michel Boucey 2011-2024

Usage: ip6addr [-v|--version]
               [(-c|--canonical) | (-n|--no-ipv4) | (-f|--full-length) |
                 (-p|--ptr) | (-w|--windows-unc) | (-r|--random)]
               [-q|--quantity ARG] [-s|--prefix ARG] [-d|--no-newline]
               [IPv6 address]

  ip6addr

Available options:
  -v,--version             Show version
  -c,--canonical           In conformation with RFC 5952 (default output)
  -n,--no-ipv4             Force the rewriting of the IPv4 address if present to
                           get a pure IPv6 address made of nibbles only
  -f,--full-length         Full IPv6 address length
  -p,--ptr                 PTR reverse mapping
  -w,--windows-unc         Windows UNC path name
  -r,--random              Random generation
  -q,--quantity ARG        Amount of random addresses to generate
  -s,--prefix ARG          Set a prefix for random addresses to generate
  -d,--no-newline          Do not output trailing newlines
  -h,--help                Show this help text
```

*N.B.* : In version 2, flags and arguments have changed from version 1, but the main features are identical.

## 4. Examples

```bash
    [user@box ~]$ ip6addr 0:0::FFFF:192.0.2.128  
    ::ffff:192.0.2.128  
    [user@box ~]$ ip6addr --no-ipv4 0:0::FFFF:192.0.2.128
    ::ffff:c000:280  
    [user@box ~]$ ip6addr --ptr 0:0::FFFF:192.0.2.128
    0.8.2.0.0.0.0.c.f.f.f.f.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.IP6.ARPA.
    [user@box ~]$ ip6addr --windows-unc 2001:db8:85a3:8d3:1319:8a2e:370:7348
    2001-db8-85a3-8d3-1319-8a2e-370-7348.ipv6-literal.net
    [user@box ~]$ ip6addr --full-length 0:0::FFFF:192.0.2.128
    0000:0000:0000:0000:0000:ffff:c000:0280
    [user@box ~]$ ip6addr --random
    ca18::654a:57ab:2ed7
    [user@box ~]$ ip6addr --random --quantity 4
    488:f7f3:7f7e::6ffd
    5b8b:8ab4::f0b8
    f191:ecc9:8193:8a2:104c:fec1:0:8c4d
    af3b:3ffd:8302:6c56::8623:26
    [user@box ~]$ ip6addr --random --quantity 3 --prefix 1234:1ab2::
    1234:1a2b::cda6:cf2a:bb1d:d70b
    1234:1a2b::362c:8295:f546:a43a  
    1234:1a2b::1121:ca16:ab5:688a  
```

