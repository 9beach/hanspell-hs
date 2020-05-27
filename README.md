# hanspell-hs
`hanspell-hs`는 (주)다음과 부산대학교 인공지능연구실/(주)나라인포테크의 웹 
서비스를 이용한 한글 맞춤법 검사기이며, 
자바스크립트로 작성한 [hanspell](https://github.com/9beach/hanspell)의
[하스켈](https://www.haskell.org/) 포트입니다.

[![Build Status](https://travis-ci.org/9beach/hanspell-hs.svg?branch=master)](https://travis-ci.org/9beach/hanspell-hs)

## 설치
[하스켈 스택](https://docs.haskellstack.org)을 이용해서 아래와 같이 
사용하세요. [해키지](https://hackage.haskell.org/)에는 곧 올릴 예정입니다.

```
$ git clone https://github.com/9beach/hanspell-hs.git
$ cd hanspell-hs
$ stack build && stack install
```

## 명령어 사용법

```
$ hanspell -h
사용법: hanspell [-d | -p | -a | -h] 

옵션:
  -d, --daum [default]	  다음 서비스를 이용해서 맞춤법을 교정합니다
  -p, --pnu               부산대학교 서비스를 이용해서 맞춤법을 교정합니다
  -a, --all 		  두 서비스의 모든 결과를 반영해서 맞춤법을 교정합니다
  -h, --info              도움말을 출력합니다
```

문장을 직접 입력하거나 클립보드에서 복사해서 맞춤법을 교정할 수 있습니다. 다음은
사용 예시입니다. <kbd>CTRL + D</kbd>는 줄을 바꾸고 맨 앞에서 입력해야 합니다.
<pre>
$ hanspell
나는 차가운 모래속에 두 손을 넣고 검게 빛나는 바다를 바라본다.
우주의 가장자리 같다.
쇼코는 해변에 서 있으면 이세상의 변두리에 선 느낌이 든다고 말했었다.
<kbd>CTRL + D</kbd>
모래속에 <font color=grey>-></font> 모래 속에<font color=grey>
띄어쓰기 오류입니다. 대치어를 참고하여 고쳐 쓰세요.</font>
이세상의 <font color=grey>-></font> 이 세상의<font color=grey>
관형사는 뒤에 오는 말과 띄어 쓰는 것이 옳습니다.
...</font>
</pre>

![스크린샷](./hanspell-screenshot.png?raw=true "한스펠 스크린샷")

파일의 맞춤법을 교정하려면 다음과 같이 명령합니다.
```
$ cat your-text | hanspell
```
로그는 생략한 채 교정된 문장만 보려면 다음과 같이 명령합니다.
```
$ cat your-text | hanspell 2> /dev/null
나는 차가운 모래 속에 두 손을 넣고 검게 빛나는 바다를 바라본다.
우주의 가장자리 같다.
쇼코는 해변에 서 있으면 이 세상의 변두리에 선 느낌이 든다고 말했었다.
```
교정 제안만 보려면 다음과 같이 명령합니다.
```
$ cat your-text | hanspell 2>&1 > /dev/null | grep '->'
```
클립보드에 복사된 문장을 입력 없이 바로 교정하려면, 맥OS 사용자는 `pbpaste`, 
X 윈도 시스템 사용자는 `xclip -o`, 마이크로소프트 윈도우 사용자는 
`powershell.exe Get-Clipboard` 명령을 이용할 수 있습니다.
```
$ pbpaste | hanspell
```
`~/.hanspell-ignore` 파일에 교정 대상에서 제외할 문자열을 
[글로브 패턴](https://ko.wikipedia.org/wiki/글로브_(프로그래밍))으로 지정할 수
있습니다. "그로떼스끄"로 시작하는 문자열과 "빠이"를 교정 대상에서 제외하려면
다음과 같이 설정하세요.
```
그로떼스끄*
빠이
```
`~/.hanspell-history` 파일에 교정 내역이 기록됩니다.
```
$ sort < ~/.hanspell-history | uniq -c | sort -nr | head 
  17 모래속에 -> 모래 속에
  13 곤색이다 -> 감색이다
  13 곤색의 -> 감색의
  13 한바퀴 -> 한 바퀴
  13 돌껀데 -> 돌 건데
  10 리랜드는 -> 이랜드는
   9 말했더만 -> 말했더구먼
   7 주름투성이 -> 주름 투성이
   7 암소여서도 -> 암소 여서도
   7 열두살 -> 열두 살
```

## 터미널 및 파일 인코딩

`hanspell`은 UTF-8으로 설정된 터미널에서만 테스트되었습니다.
```
$ cat your-text.utf-8 | hanspell
```

홈 디렉터리의 `.hanspell-ignore` 파일 또한 UTF-8 인코딩으로 저장해야 합니다.

## 라이브러리 사용법

다음은 간단한 예제입니다. 조금 복잡한 사용례로
[app/Main.hs](https://github.com/9beach/hanspell-hs/blob/master/app/Main.hs)가
있습니다.
```haskell
import Language.Hanspell

main = do
    let sentence = "위에계신분, 잘들리세요?"
    let correctSentence = "위에 계신 분, 잘 들리세요?"
    typos <- spellCheckByDaum sentence
    let fixedSentence = fixTyposWithStyle False sentence typos
    putStrLn . show $ fixedSentence == correctSentence
```
