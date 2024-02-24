# internet-archive

## Introduction

This simple Emacs package lets you download any PDF file from the [Internet Archive](https://archive.org/) seamlessly, as long as it can be borrowed for at least one hour.

## Requirements

- [ia](https://archive.org/developers/internetarchive/cli.html).
- [wget](https://www.gnu.org/software/wget/).
- [Adobe Digital Editions](https://www.adobe.com/solutions/ebook/digital-editions.html).
- [calibre](https://calibre-ebook.com/) with [DRM Removal plugin](https://www.epubor.com/calibre-drm-removal-plugins.html).

If you are on macOS, you can install all of the above requirements by running

```shell
brew install internetarchive wget adobe-digital-editions calibre
```

## Installation

### Manual installation

Clone this repository and add this to your `init.el` file:

``` emacs-lisp
(add-to-list 'load-path "path/to/internet-archive")
```

Where `"path/to/internet-archive"` is the path to the local repository you just cloned.

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

## One-time setup

When running for the first time:

1. *Configure the `ia`*: run `ia configure` and follow the instructions.
2. *Export your IA cookies file*. You can export the cookies by installing the [Get cookies.txt LOCALLY](https://github.com/kairi003/Get-cookies.txt-LOCALLY) browser extension. Then go to https://archive.org/, click on the extension, click on ‘export’, and save it to `~/.config/cookies.txt`. (If you would like to save it to different location, you need to manually set `internet-archive-cookies-file`.)

## User options

- Depending on where in your file system the relevant Calibre and Adobe Digital Editions are found, you may need to set the values of `internet-archive-calibre-directory` and `internet-archive-ade-directory` accordingly.

- Emacs should be able to find the `ia`, `wget` and `calibredb` executables. But if it doesn’t, you can specify their location manually by setting the value of `internet-archive-cli-file`, `internet-archive-wget-file` and `internet-archive-calibredb-file`, respectively.

- If you want Adobe Digital Editions to be closed once it is done downloading the PDF from the Internet Archive, set `internet-archive-ade-close-when-done` to `t`.  If you want Adobe Digital Editions to open in the background, set `internet-archive-ade-open-in-background` to `t`. Note that it seems like ADE will start downloading the file only when
it is in the foreground, so this option may be less useful than it appears.

- For the full list of user options, `M-x customize-group RET internet-archive`.

## Usage

### From Emacs

`M-x internet-archive`, followed by a URL or a title, depending on whether you already know the URL of the book you would like to download or need to search for it. If you choose to search for a book, you will also be prompted to enter an author. The `title` and `author` fields are each, but not jointly, optional.

### From the browser 

If you have `org-protocol` [installed](https://www.orgroam.com/manual.html#Installation-_00281_0029) and configured on your system, you can also trigger the function directly from your web browser, by creating a bookmark with the following JavaScript code:

``` javascript
javascript:location.href='org-protocol://internet-archive?url=%27 + encodeURIComponent(location.href);
```

Then simply click on this bookmark after clicking ‘Borrow for 1 hour’.

## FAQ

***I see more results when I run a search on the Internet Archive website than when I use your package.***

The search results we display are deliberately restricted to *books available for borrowing*. In addition, the search terms will only match results in the associated fields (`author` or `title`). So e.g. if you enter “borges” when prompted for an author, this will match *El aleph*, but will not match *[Borges](https://archive.org/details/borges-adolfo-bioy-casares)* (because Borges is not the author), *[Borges por él mismo](https://archive.org/details/cd_jorge-luis-borges-lee-sus-poemas_jorge-luis-borges)* (because it is not a book) or *[Ficciones](https://archive.org/details/obrascompletas0000borg)* (because it is not currently borrowable).

***“This book is not available to borrow at this time. Please try again later.”***

This sometimes happens with books that *are* available for borrowing. It appears to be a limitation of the Internet Archive CLI. If this happens, please go to the website, borrow the book manually, then run `internet-archive` again with the URL.

## Troubleshooting

The package has not been extensively tested. If you encounter any problems, feel free to open an issue.
