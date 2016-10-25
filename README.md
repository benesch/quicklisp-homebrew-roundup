# quicklisp-roundup

[Homebrew][homebrew] has a strict policy against install scripts that
download things. ([See rationale.][install-script-rationale]) This means
using Quicklisp to install your dependencies in your install script is out
of the question.

Instead, you'll have to explicitly vendor each dependency. See the
[Ansible formula][ansible-formula] for an example.

Once you depend on more than, like, two Common Lisp packages, this is a major
pain.

quicklisp-roundup mostly automates this for you. Given an ASDF system
available through Quicklisp, it will resolve all necessary systems and attempt
to locate the encompassing Quicklisp releases. The release tarballs are
downloaded, checksummed, and printed out for you in Homebrew's DSL, ready
for copy/paste into your formula:

```ruby
resource '<system>' do
   url 'http://beta.quicklisp.org/...tar.gz'
   sha1 '3fae8409bd3ef76...'
end
...
```

Alternatively, quicklisp-roundup can generate a tarball containing all dependent
systems. This is useful if you have enough dependencies that listing them
individually is impractical.

## Usage

In both modes, you'll be notified of any *missing* systems—dependencies which
aren't available in Quicklisp. You'll have to a) close your eyes and pray, or b)
manually vendor those systems.

### Homebrew resource blocks

```lisp
* (ql:quickload "quicklisp-roundup")
* (quicklisp-roundup:make-homebrew "<system>")
; Resolving "pgloader":
;   Missing 1 system!
;     asdf
;   Included 59 systems:
;     abnf alexandria anaphora asdf-finalizers
;     asdf-system-connections...
;
; Homebrew formula block written to:
;   "build/pgloader.rb"
```


```ruby
# build/pgloader.rb

resource 'alexandria' do
  url 'http://beta.quicklisp.org/archive/alexandria/2013-01-28/alexandria-20130128-git.tgz'
  sha1 '89e303120c1ceb266cf6ae3cb12f75c119a85ed1'
end

resource 'anaphora' do
  url 'http://beta.quicklisp.org/archive/anaphora/2011-06-19/anaphora-0.9.4.tgz'
  sha1 '1e9faa00efff11b45ca7bed64a1fb60e9ce55dbd'
end

# ...
```

### Tarball

```lisp
* (ql:quickload "quicklisp-roundup")
* (quicklisp-roundup:make-tarball "<system>")
; Resolving "pgloader":
;   Missing 1 system!
;     asdf
;   Included 59 systems:
;     abnf alexandria anaphora asdf-finalizers
;     asdf-system-connections...
;
; Tarball written to:
;   "build/pgloader.tgz"
```

## Notes

I don't speak Lisp. No, really, I don't! Improvements and criticisms welcomed.

Only tested with [Steel Bank Common Lisp](http://www.sbcl.org).

If you find a bug, file it away in the [GitHub issue tracker][issues]!


## Author

Nikhil Benesch <nikhil.benesch@gmail.com>

[ansible-formula]: https://github.com/Homebrew/homebrew-core/blob/master/Formula/ansible.rb#L23
[homebrew]: https://brew.sh
[install-script-rationale]:
https://github.com/Homebrew/brew/blob/master/docs/Formula-Cookbook.md#specifying-gems-python-modules-go-projects-etc-as-dependencies
[issues]: https://github.com/benesch/quicklisp-homebrew-roundup/issues
