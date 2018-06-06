# Windows

```ps1
# Delete .emacs.d
Remove-Item $env:APPDATA\.emacs.d -Force -Recurse -ErrorAction SilentlyContinue
Remove-Item $env:APPDATA\.emacs -Force -Recurse -ErrorAction SilentlyContinue

git clone git@github.com:m-ghaoui/emacs.git $env:APPDATA\.emacs.d
```

# Unix

```sh
rm -rvf ~/.emacs.d
rm -rvf ~/.emacs

git clone git@github.com:m-ghaoui/emacs.git ~/.emacs.d
```
