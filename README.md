# hanspell.hs

[hanspell](https://github.com/9beach/hanspell)의 [하스켈](https://www.haskell.org/) 
포트입니다. [해키지](https://hackage.haskell.org/)에 올리기 전까지 자세한 사용법은 
[이 문서](https://github.com/9beach/hanspell/blob/master/README.md)를 참고하세요. 
자바스크립트 버전에 비해 아직은 지원하지 않는 기능이 있습니다. 

```
$ git clone https://github.com/9beach/hanspell.hs.git
$ cd hanspell.hs
$ cabal build
$ cp dist/build/hanspell/hanspell to-your-favorite-bin-path
```

```
$ cabal haddock
$ # check hanspell documentations in dist/doc/html/hanspell
```