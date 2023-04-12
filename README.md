# hanspell-hs

`hanspell-hs`는 (주)다음과 부산대학교 인공지능연구실/(주)나라인포테크의 웹 서비스를 이용한 한글 맞춤법 검사기입니다.

[비주얼 스튜디오 코드 한스펠](https://github.com/9beach/vscode-hanspell)과 자바스크립트로 작성한 [hanspell](https://github.com/9beach/hanspell)도 있으니
참고하세요.

[![Hackage](https://img.shields.io/hackage/v/hanspell.svg)](https://hackage.haskell.org/package/hanspell)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/hanspell.svg)

## 설치

[한스펠 릴리스](https://github.com/9beach/hanspell-hs/releases)에서 실행파일을 다운로드 하세요. 소스를 받아서 설치하려면 [카발](https://www.haskell.org/cabal/)을 이용해서 아래와 같이 설치하세요.

```sh
cabal update && cabal install hanspell
```

[스택](https://docs.haskellstack.org)을 이용해서 다음과 같이 설치할 수도 있습니다.

```sh
stack update && stack install hanspell
```

## 명령어 사용법

```console
$ hanspell -h
사용법: hanspell [-d | -p | -a | -h]

옵션:
  -d, --daum [default]	  다음 서비스를 이용해서 맞춤법을 교정합니다
  -p, --pnu               부산대학교 서비스를 이용해서 맞춤법을 교정합니다
  -a, --all               두 서비스의 모든 결과를 반영해서 맞춤법을 교정합니다
  -h, --info              도움말을 출력합니다

버그 리포트와 제안: <https://github.com/9beach/hanspell-hs/issues>
한스펠 홈 페이지: <https://github.com/9beach/hanspell-hs/>
```

문장을 직접 입력하거나 클립보드에서 복사해서 맞춤법을 교정할 수 있습니다. 다음은
사용 예시입니다. `CTRL + D`는 줄을 바꾸고 맨 앞에서 입력해야 합니다.

```console
$ hanspell
나는 차가운 모래속에 두 손을 넣고 검게 빛나는 바다를 바라본다.
우주의 가장자리 같다.
쇼코는 해변에 서 있으면 이세상의 변두리에 선 느낌이 든다고 말했었다.
[CTRL + D]
모래속에 -> 모래 속에
띄어쓰기 오류입니다. 대치어를 참고하여 고쳐 쓰세요.
이세상의 -> 이 세상의
관형사는 뒤에 오는 말과 띄어 쓰는 것이 옳습니다.
...
```

![스크린샷](https://github.com/9beach/hanspell-hs/blob/master/hanspell-screenshot.png?raw=true "한스펠 스크린샷")

파일의 맞춤법을 교정하려면 다음과 같이 명령합니다.

```sh
cat your-text | hanspell
```

로그는 생략한 채 교정된 문장만 보려면 다음과 같이 명령합니다.

```console
$ cat your-text | hanspell 2> /dev/null
나는 차가운 모래 속에 두 손을 넣고 검게 빛나는 바다를 바라본다.
우주의 가장자리 같다.
쇼코는 해변에 서 있으면 이 세상의 변두리에 선 느낌이 든다고 말했었다.
```

교정 제안만 보려면 다음과 같이 명령합니다.

```sh
cat your-text | hanspell 2>&1 > /dev/null | grep '->'
```

클립보드에 복사된 문장을 교정하려면, macOS 사용자는 `pbpaste`,
X 윈도 시스템 사용자는 `xclip -o`, 마이크로소프트 윈도우 사용자는
`powershell.exe Get-Clipboard` 명령을 이용할 수 있습니다.

```sh
pbpaste | hanspell
```

`~/.hanspell-ignore` 파일에 교정 대상에서 제외할 문자열을
[글로브 패턴](<https://ko.wikipedia.org/wiki/글로브_(프로그래밍)>)으로 지정할 수
있습니다. ‘그로떼스끄’로 시작하는 문자열과 ‘빠이’를 교정 대상에서 제외하려면
다음과 같이 설정하세요.

```txt
그로떼스끄*
빠이
```

`~/.hanspell-history` 파일에는 맞춤법 교정 내용이 기록됩니다.

```txt
내노라하는 -> 내로라하는
전세계 -> 전 세계
그 뿐만 -> 그뿐만
때 마다 -> 때마다
했는 지 -> 했는지
...
```

아래는 사용자가 자주 틀리는 맞춤법을 빈도순으로 보여주는 셸 스크립트입니다.

```console
$ sort < ~/.hanspell-history | uniq -c | sort -nr | head -n 5
  17 모래속에 -> 모래 속에
  13 그 뿐만 -> 그뿐만
  13 했는 지 -> 했는지
  13 한바퀴 -> 한 바퀴
   7 내노라하는 -> 내로라하는
```

## 터미널 및 파일 인코딩

`hanspell`은 UTF-8으로 설정된 터미널에서만 테스트되었습니다.

```sh
cat your-text.utf-8 | hanspell
```

홈 디렉터리의 `.hanspell-ignore` 파일 또한 UTF-8 인코딩으로 저장해야 합니다.

## 라이브러리 사용법

[Language.Hanspell](https://hackage.haskell.org/package/hanspell/docs/Language-Hanspell.html) 라이브러리는
[Typo](https://hackage.haskell.org/package/hanspell/docs/Language-Hanspell.html#t:Typo) 자료구조와 관련 함수, 그리고
[spellCheckByDaum](https://hackage.haskell.org/package/hanspell/docs/Language-Hanspell.html#v:spellCheckByDaum),
[spellCheckByPnu](https://hackage.haskell.org/package/hanspell/docs/Language-Hanspell.html#v:spellCheckByPnu)
함수를 제공합니다. 다음은 사용 예입니다.

```haskell
module HanspellExample where

import Language.Hanspell

example = do
    let sentence = "위에계신분, 잘들리세요?"
    typos <- spellCheckByDaum sentence
    mapM_ (putStrLn . typoToStringWithStyle True) typos
```

다음의 결과가 예상됩니다.

```txt
위에계신분, -> 위에 계신 분,
뒤에 오는 명사를 수식하는 관형격 어미 ‘-ㄴ’, ‘-는’, ‘-던’, ‘-ㄹ’ 등과 의존명사는 띄어 쓰는 것이 옳습니다.
(예)
노력한 만큼 대가를 얻다.
소문으로만 들었을 뿐이네.
합격했다는 소리를 들으니 그저 기쁠 따름이다.

잘들리세요? -> 잘 들리세요?
'익숙하고 능란하게', '좋고 훌륭하게'라는 의미의 부사 '잘'은 띄어 쓰세요.
(예)
바둑을 잘 두다.
옷을 잘 차려입고 나서니 딴사람 같구나.
다음 대화를 잘 듣고 물음에 답하세요.
```

## 라이선스 고지

이 프로그램의 소스 코드는 MIT 라이선스를 따르지만, 부산대학교 인공지능연구실/(주)나라인포테크의 맞춤법 웹 서비스는 권리자가 고지한 대로 개인이나 학생만 무료로 사용할 수 있습니다.
