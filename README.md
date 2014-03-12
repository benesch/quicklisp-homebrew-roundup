# quicklisp-homebrew-roundup

[Homebrew][homebrew] has a strict policy against install scripts that
download things. ([See rationale.][install-script-rationale]) This means
using Quicklisp to install your dependencies in your install script is out
of the question.

Instead, you'll have to explicitly vendor each dependency. See the
[Ansible formula][ansible-formula] for an example.

Once you depend on more than, like, two Common Lisp packages, this is a major
pain in the rear.

quicklisp-homebrew-roundup mostly automates this for you. Given an ASDF system
available through Quicklisp, it will resolve all necessary systems and attempt
to locate the encompassing Quicklisp releases. The release tarballs are
downloaded, checksummed, and printed out for you in Homebrew's DSL, ready
for copy/paste into your formula:

```
resource '<system>' do
   url 'http://beta.quicklisp.org/...tar.gz'
   sha1 '3fae8409bd3ef76...'
end
...
```

## Usage

```
(ql:quickload "quicklisp-homebrew-roundup")
(quicklisp-homebrew-roundup:for-system "<system>")
```

You'll be notified of any *missing* systems—dependencies which aren't available
in Quicklisp. You'll have to a) close your eyes and pray, or b) manually vendor
those systems.

## Example

```
* (quicklisp-homebrew-roundup:for-system "pgloader")
Resolving "pgloader":
  Missing 1 system!
    asdf
  Included 59 systems:
    abnf alexandria anaphora asdf-finalizers
    asdf-system-connections babel bordeaux-threads cffi
    chipz chunga cl+ssl cl-base64 cl-containers cl-csv
    cl-fad cl-interpol cl-log cl-markdown cl-postgres
    cl-ppcre cl-unicode closer-mop com.informatimago.clext
    com.informatimago.clext.association
    com.informatimago.common-lisp.cesarum
    com.informatimago.common-lisp.lisp-sexp
    command-line-arguments db3 drakma dynamic-classes esrap
    flexi-streams ironclad iterate list-of local-time
    lparallel md5 metabang-bind metatilities-base nibbles
    parse-number pgloader postmodern puri py-configparser
    qmynd s-sql salza2 simple-date split-sequence sqlite
    trivial-backtrace trivial-features trivial-garbage
    trivial-gray-streams uiop usocket zip
; Fetching #<URL "http://beta.quicklisp.org/archive/asdf-finalizers/2013-06-15/asdf-finalizers-20130615-git.tgz">
; 6.46KB
==================================================
6,616 bytes in 0.01 seconds (587.36KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/archive/asdf-system-connections/2014-02-11/asdf-system-connections-20140211-git.tgz">
; 4.85KB
...
Homebrew formula block:

  resource 'alexandria' do
    url 'http://beta.quicklisp.org/archive/alexandria/2013-01-28/alexandria-20130128-git.tgz'
    sha1 '89e303120c1ceb266cf6ae3cb12f75c119a85ed1'
  end

  resource 'anaphora' do
    url 'http://beta.quicklisp.org/archive/anaphora/2011-06-19/anaphora-0.9.4.tgz'
    sha1 '1e9faa00efff11b45ca7bed64a1fb60e9ce55dbd'
  end

  resource 'asdf-finalizers' do
    url 'http://beta.quicklisp.org/archive/asdf-finalizers/2013-06-15/asdf-finalizers-20130615-git.tgz'
    sha1 '5f3fe1a3b68b6e541e88eaa9d7eb1ec7c1c29eb1'
  end

  resource 'asdf-system-connections' do
    url 'http://beta.quicklisp.org/archive/asdf-system-connections/2014-02-11/asdf-system-connections-20140211-git.tgz'
    sha1 '212ab6d6e591c106ebab26e000a9cf6cf41d3022'
  end

  resource 'babel' do
    url 'http://beta.quicklisp.org/archive/babel/2013-03-12/babel-20130312-git.tgz'
    sha1 '7d917cfaacf293ab6593a98d42ca1c6954210e82'
  end

  resource 'bordeaux-threads' do
    url 'http://beta.quicklisp.org/archive/bordeaux-threads/2013-06-15/bordeaux-threads-0.8.3.tgz'
    sha1 'c135e9149d524020572b08e884ebb3a2eeed50b3'
  end
...
```

## Notes

I don't speak Lisp. No, really, I don't! Improvements and criticisms welcomed.

If you find a bug, file it away in the [GitHub issue tracker][issues]!


## Author

Nikhil Benesch <nikhil.benesch@gmail.com>

[ansible-formula]: https://github.com/Homebrew/homebrew/blob/master/Library/Formula/ansible.rb#L15
[homebrew]: https://brew.sh
[install-script-rationale]:
https://github.com/Homebrew/homebrew/wiki/Formula-Cookbook#wiki-specifying-gems-python-modules-etc-as-dependencies
[issues]: https://github.com/benesch/quicklisp-homebrew-roundup/issues
