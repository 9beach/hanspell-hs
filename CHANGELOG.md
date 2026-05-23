# Change Log

## [1.0.0] - 2026-05-23

### 변경된 기능

- 부산대학교 인공지능연구실/(주)나라인포테크의 맞춤법 검사기 서비스가
  종료/차단되어 제거. 대신 네이버(주) 맞춤법 검사기 지원 추가
  (`Language.Hanspell.NaverSpellChecker`, `spellCheckByNaver`, `-n` / `--naver`).
- 빌드 시스템을 Cabal로 통일 (Stack/hpack 의존성 제거).
- GHC 9.6 + 최신 의존성(text 2.x, aeson 2.x 등) 호환.
- HTTP 요청에 20초 타임아웃 설정.
- `Debug.Trace.trace`로 출력하던 사용자 메시지를 `hPutStrLn stderr`로 전환.
- 정규식 패키지를 `regex-compat-tdfa`에서 `regex-tdfa`로 일원화.
