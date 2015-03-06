package cc.factorie.app.nlp.segment.es;

import java.io.Reader;
import java.util.logging.Logger;
import java.util.Properties;

%%

%class SmallJavaLexer
%unicode
%function next
%type Object
%char
%caseless

%{
  public SmallJavaLexer(Reader r, Properties props) {
    this(r);
    }

      private Object getNext() {
        final String txt = yytext();
        return getNext(txt, txt);
      }

      private Object getNext(String txt, String originalText) {
        return getNext(txt, originalText, null);
      }

      private Object getNext(String txt, String originalText, String annotation) {
        return new int[]{yychar, yylength()};
      }
%}


SPLET = &[aeiouAEIOU](acute|grave|uml)

/* For some reason U+0237-U+024F (dotless j) isn't in [:letter:]. Recent additions? */
CHAR = [:letter:]|{SPLET}|[\u00AD\u0237-\u024F\u02C2-\u02C5\u02D2-\u02DF\u02E5-\u02FF\u0300-\u036F\u0370-\u037D\u0384\u0385\u03CF\u03F6\u03FC-\u03FF\u0483-\u0487\u04CF\u04F6-\u04FF\u0510-\u0525\u055A-\u055F\u0591-\u05BD\u05BF\u05C1\u05C2\u05C4\u05C5\u05C7\u0615-\u061A\u063B-\u063F\u064B-\u065E\u0670\u06D6-\u06EF\u06FA-\u06FF\u070F\u0711\u0730-\u074F\u0750-\u077F\u07A6-\u07B1\u07CA-\u07F5\u07FA\u0900-\u0903\u093C\u093E-\u094E\u0951-\u0955\u0962-\u0963\u0981-\u0983\u09BC-\u09C4\u09C7\u09C8\u09CB-\u09CD\u09D7\u09E2\u09E3\u0A01-\u0A03\u0A3C\u0A3E-\u0A4F\u0A81-\u0A83\u0ABC-\u0ACF\u0B82\u0BBE-\u0BC2\u0BC6-\u0BC8\u0BCA-\u0BCD\u0C01-\u0C03\u0C3E-\u0C56\u0D3E-\u0D44\u0D46-\u0D48\u0E30-\u0E3A\u0E47-\u0E4E\u0EB1-\u0EBC\u0EC8-\u0ECD]
WORD = ({CHAR})+
SPACE = \s+


%%

cannot			{ yypushback(3) ; return getNext(); }

{WORD}			{ return getNext(); }
{SPACE}           { return null;}

<<EOF>>  { return null; }

