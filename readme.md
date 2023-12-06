# internet-archive

## Introduction

This simple Emacs package lets you download any PDF from the [Internet Archive](https://archive.org/), as long as it can be borrowed for one hour (or longer).

## Requirements

- [wget](https://www.gnu.org/software/wget/).
- [Adobe Digital Editions](https://www.adobe.com/solutions/ebook/digital-editions.html).
- [calibre](https://calibre-ebook.com/) with [DRM Removal plugin](https://www.epubor.com/calibre-drm-removal-plugins.html).

## Installation

### Manual installation

Clone this repository and add this to your `init.el` file:

``` emacs-lisp
(add-to-list 'load-path "path/to/internet-archive")
```

where `"path/to/internet-archive"` is the path to the local repository you just cloned.

### Elpaca/Straight

If you use the [elpaca](https://github.com/progfolio/elpaca) package manager, you just need to add this your `init.el` file:

``` emacs-lisp
(use-package internet-archive
  :elpaca (internet-archive
           :host github
	   :repo "benthamite/internet-archive")
  :demand t)
```

If you use [straight](https://github.com/radian-software/straight.el), just replace `:elpaca` with `:straight` in the formula above.

## Configuration

1. Since `wget` needs to authenticate to be able to download files which you have borrowed from the Internet Archive, you must first export your IA cookies file and set `internet-archive-cookies-file` to point to this file. You can download the cookies by installing the [Get cookies.txt LOCALLY](https://github.com/kairi003/Get-cookies.txt-LOCALLY) browser extension. Then go to https://archive.org/, click on the extension, then click on ‘export’.

2. Depending on where in your file system the relevant Calibre and Adobe Digital Editions are found, you may need to set the values of `internet-archive-calibre-directory` and `internet-archive-adobe-digital-editions-directory` accordingly.

3. If `wget` or `calibredb` are not in your PATH, you will also have to set the values of `internet-archive-wget` or `internet-archive-calibredb`.

## Usage

1. From the page of the book you would like to download, click ‘Borrow for 1 hour’.

2. Copy the URL.

3. `M-x internet-archive-download RET`.

### org-protocol

If you have `org-protocol` [installed](https://www.orgroam.com/manual.html#Installation-_00281_0029) and configured on your system, you can also trigger the function directly from your browser, by creating a bookmark with the following JavaScript code:

``` javascript
javascript:location.href='org-protocol://internet-archive?url=%27 + encodeURIComponent(location.href);
```

Then simply click on this bookmark after after step (1) above.

## Troubleshooting

The package has not been extensively tested. If you encounter any problems, feel free to open an issue.
