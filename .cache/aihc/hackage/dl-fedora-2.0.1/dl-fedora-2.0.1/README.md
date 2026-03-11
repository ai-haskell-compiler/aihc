# dl-fedora

[![Hackage](https://img.shields.io/hackage/v/dl-fedora.svg)](https://hackage.haskell.org/package/dl-fedora)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/dl-fedora/badge/lts)](http://stackage.org/lts/package/dl-fedora)
[![Stackage Nightly](http://stackage.org/package/dl-fedora/badge/nightly)](http://stackage.org/nightly/package/dl-fedora)

A tool for downloading Fedora, ELN, and Centos Stream images.
By default it targets the Workstation edition of Fedora.

Usage examples:

`dl-fedora rawhide` : downloads the latest Fedora Rawhide Workstation Live iso

`dl-fedora 42 silverblue` : downloads the Fedora Silverblue iso

`dl-fedora respin kde` : downloads the latest KDE Live respin

`dl-fedora 41 --list` : list editions for version

`dl-fedora 42 kde workstation` : download both KDE and Workstation editions

`dl-fedora 41 server --arch aarch64` : will download the Server iso for armv8

`dl-fedora --run 42` : will download Fedora Workstation and boot the Live image with qemu-kvm.

`dl-fedora --check respin` : checks if there is a newer respin iso image
available.

`dl-fedora --local rawhide` : shows the current locally available image.
It can be combined with `--run` to quickly run the latest local image,
without a newer download.

`dl-fedora c10s` : downloads a Centos Stream net installer.

By default dl-fedora downloads to `~/Downloads/`
(correctly the XDG user "DOWNLOAD" directory), or `~/Downloads/iso/` if it exists.
If `~/Downloads/` doesn't exist then the current directory is used.
You can override the download directory with `--dir DIR` (which can be relative: eg `.`).
Curl downloads to a `.dl-fedora-partial` subdirectory first (which can safely be removed when not using dl-fedora)
and the downloaded file is then moved up to the actual download directory when complete.

`dl-fedora` downloads the latest mirrored image redirected from
`download.fedoraproject.org` by default.
If you want to ensure getting the very latest image you can use `--latest`,
which will then download from `dl.fedoraproject.org` instead
_if_ your mirror is not synced yet.

If the image is already found to be downloaded
it will not be re-downloaded of course (unless you use a different `--dir` say).
Curl is used to do the downloading: partial downloads will continue.

A symlink to the latest iso is also created:
eg for rawhide it might be `"Fedora-Workstation-Live-x86_64-Rawhide-latest.iso"`.

It also tries to check the iso checksum and its gpg signature.

There are some release aliases:
- {`8`,`9`,`10`} are aliases for {`c8s`, `c9s`, `c10s`}
- `{8,9,10}-{live,respin}` are aliases for `{c8s,c9s,c10s}-{live,respin}`
- currently `11` is an alias for eln
- the current rawhide version can be used for `rawhide`
- `next` is an alias for the next Fedora release (either Rawhide or "branched")
- `current` is an alias for the latest current stable Fedora release (though you may prefer newer `respin`)
- `previous` is an alias for the previous current stable Fedora release

Also a few edition abbreviations:
- `ws` and `gnome` are aliases for `workstation` (default edition)
- `sb` is short for `silverblue`.

## Usage
`$ dl-fedora --version`

```
2.0.1
```
`$ dl-fedora --help`

```
Fedora iso downloader

Usage: dl-fedora [--version] [-g|--gpg-keys] [--no-checksum | --checksum] 
                 [--debug] [-T|--no-http-timeout] 
                 [(-c|--check) | (-l|--local) | --list | (-R|--replace)] 
                 [-d|--dir DIRECTORY] [-n|--dry-run] [-r|--run] 
                 [(-L|--latest) | --dl | (-k|--koji) | (-m|--mirror URL)] 
                 [--dvd] [--cs-devel | --cs-test | --cs-production] 
                 [--alt-cs-extra-edition ('MAX'|'MIN')] [-a|--arch ARCH] RELEASE
                 [--all-desktops | --all-spins | --all-editions | [-x|--exclude]
                   [EDITION...]]

  Tool for downloading Fedora iso file images.
  RELEASE = release number, respin, rawhide, test (Beta), stage (RC), eln, c9s, c10s, c9s-live
  EDITION = {cloud,container,everything,kde,server,workstation,budgie,cinnamon,
             cosmic,i3,kdemobile,lxde,lxqt,mate,miracle,soas,sway,xfce,
             silverblue,kinoite,onyx,sericea,iot} [default: workstation]

  See <https://github.com/juhp/dl-fedora/#readme>

Available options:
  -h,--help                Show this help text
  --version                Show version
  -g,--gpg-keys            Import Fedora GPG keys for verifying checksum file
  --no-checksum            Do not check checksum
  --checksum               Do checksum even if already downloaded
  --debug                  Debug output
  -T,--no-http-timeout     Do not timeout for http response
  -c,--check               Check if newer image available
  -l,--local               Show current local image
  --list                   List spins and editions
  -R,--replace             Delete previous snapshot image after downloading
                           latest one
  -d,--dir DIRECTORY       Download directory [default: ~/Downloads/iso]
  -n,--dry-run             Don't actually download anything
  -r,--run                 Boot image in QEMU
  -L,--latest              Get latest image either from mirror or dl.fp.o if
                           newer
  --dl                     Use dl.fedoraproject.org (dl.fp.o)
  -k,--koji                Use koji.fedoraproject.org
  -m,--mirror URL          Mirror url for /pub [default
                           https://download.fedoraproject.org/pub]
  --dvd                    Download dvd iso instead of boot netinst (for Server,
                           eln, centos)
  --cs-devel               Use centos-stream development compose
  --cs-test                Use centos-stream test compose
  --cs-production          Use centos-stream production compose (default is
                           mirror.stream.centos.org)
  --alt-cs-extra-edition ('MAX'|'MIN')
                           Centos Stream Alternative Live Spin editions
                           (MAX,MIN)
  -a,--arch ARCH           Specify arch [default: x86_64]
  --all-desktops           Get all Fedora desktops
  --all-spins              Get all Fedora Spins
  --all-editions           Get all Fedora editions
  -x,--exclude             Exclude specified editions
```

## References
See <https://fedoraproject.org/wiki/Infrastructure/MirrorManager>,
<https://admin.fedoraproject.org/mirrormanager/>,
and also <https://fedoramagazine.org/verify-fedora-iso-file>.

## Contribution
dl-fedora is distributed under the GPL license version 3 or later.

Please report issues or pull requests at <https://github.com/juhp/dl-fedora>.
