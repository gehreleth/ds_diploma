package make_corpus;

import opennlp.tools.namefind.NameFinderME;
import opennlp.tools.namefind.TokenNameFinderModel;
import opennlp.tools.sentdetect.SentenceDetectorME;
import opennlp.tools.sentdetect.SentenceModel;
import opennlp.tools.stemmer.PorterStemmer;
import opennlp.tools.tokenize.Tokenizer;
import opennlp.tools.tokenize.TokenizerME;
import opennlp.tools.tokenize.TokenizerModel;
import opennlp.tools.util.Span;

import java.io.*;
import java.util.StringTokenizer;
import java.util.regex.Pattern;

public class Startup {

    public static final String URL_SUBST = "5446fb31b66c149c85a381b19c94344a90f6574c";
    public static final String CURR_SUBST = "90540a48b75388cd11d3c82714e1444ad4c84268";
    public static final String PERSON_SUBST = "712c71ac36a11b2f8bdf0d1e3e360ed4d578e9c9";

    public static void main(String[] args) throws IOException {
        InputStream sentenceModelModelIn = null;
        InputStream tokenNameFinderModelIn = null;
        InputStream tokenizerModelIn = null;
        try {
            Models models = new Models();
            sentenceModelModelIn = new FileInputStream("en-sent.bin");
            models.sentenceModel = new SentenceModel(sentenceModelModelIn);
            tokenizerModelIn = new FileInputStream("en-token.bin");
            models.tokenizerModel = new TokenizerModel(tokenizerModelIn);
            tokenNameFinderModelIn = new FileInputStream("en-ner-person.bin");
            models.tokenNameFinderModel = new TokenNameFinderModel(tokenNameFinderModelIn);

            System.out.println("../src_data/en_US/en_US.blogs.txt");
            processSingleFile(models, "../src_data/en_US/en_US.blogs.txt", "../src_data/en_US/en_US.blogs.pp.txt");
            System.out.println("../src_data/en_US/en_US.news.txt");
            processSingleFile(models, "../src_data/en_US/en_US.news.txt", "../src_data/en_US/en_US.news.pp.txt");
            System.out.println("../src_data/en_US/en_US.twitter.txt");
            processSingleFile(models, "../src_data/en_US/en_US.twitter.txt", "../src_data/en_US/en_US.twitter.pp.txt");
        } finally {
            if (tokenNameFinderModelIn != null) {
                try {
                    tokenNameFinderModelIn.close();
                } catch (Exception e) {
                }
            }
            if (tokenizerModelIn != null) {
                try {
                    tokenizerModelIn.close();
                } catch (Exception e) {
                }
            }
            if (sentenceModelModelIn != null) {
                try {
                    sentenceModelModelIn.close();
                } catch (Exception e) {
                }
            }
        }
    }

    public static String replaceAll(String source, CharSequence seq, CharSequence replacement) {
        int p1, p0 = 0;
        String seq0 = String.valueOf(seq);
        StringBuilder retVal = new StringBuilder();
        do {
            p1 = source.indexOf(seq0, p0);
            retVal.append(p1 != -1 ? source.substring(p0, p1) : source.substring(p0));
            if (p1 != -1) {
                retVal.append(replacement);
                p0 = p1 + seq.length();
            } else
                p0 = p1;
        } while (p0 != -1);
        return retVal.toString();
    }

    public static String replaceAllNames(Tokenizer tokenizer, NameFinderME nameFinder, String source, CharSequence repl) {
        String[] sentence = tokenizer.tokenize(source);
        Span nameSpans[] = nameFinder.find(sentence);
        nameFinder.clearAdaptiveData();
        String s0 = source;
        if (nameSpans.length != 0) {
            for (Span span: nameSpans) {
                StringBuilder sb = new StringBuilder();
                boolean first = true;
                for (int i = span.getStart(); i < span.getEnd(); ++i ) {
                    if (!first)
                        sb.append("\\s+");
                    else
                        first = false;
                    sb.append(Pattern.quote(sentence[i]));
                }
                s0 = s0.replaceAll(sb.toString(), String.valueOf(repl));
            }
            return s0;
        } else {
            return source;
        }
    }

    public static void processSingleFile(Models models, String inputFileName, String outputFileName) throws IOException {
        BufferedReader reader = null;
        BufferedWriter writer = null;
        try {
            SentenceDetectorME sentenceDetector = new SentenceDetectorME(models.sentenceModel);
            Tokenizer tokenizer = new TokenizerME(models.tokenizerModel);
            NameFinderME nameFinder = new NameFinderME(models.tokenNameFinderModel);
            reader = new BufferedReader(new InputStreamReader(new FileInputStream(inputFileName)));
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFileName)));
            PorterStemmer stemmer = new PorterStemmer();
            int count = 0;
            while (true) {
                String line = reader.readLine();
                if (line == null)
                    break;
                String[] sentences = sentenceDetector.sentDetect(line);
                for (final String sentence : sentences) {
                    System.out.printf("Sentence : %d\n", ++count);
                    String s0 = sentence;
                    s0 = replaceAllNames(tokenizer, nameFinder, s0, PERSON_SUBST);

                    s0 = s0.replaceAll("https?://(www\\.)?[-a-zA-Z0-9@:%._+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_+.~#?&//=]*)",
                            URL_SUBST);

                    s0 = replaceAll(s0, "\u201c", "\"");
                    s0 = replaceAll(s0, "\u201d", "\"");
                    s0 = replaceAll(s0, "\u2019", "\'");

                    s0 = s0.toLowerCase();
                    s0 = replaceAll(s0, "'m", " am");
                    s0 = replaceAll(s0, "'re", " are");
                    s0 = replaceAll(s0, "'s", " is");
                    s0 = replaceAll(s0, "can't", "can not");
                    s0 = replaceAll(s0, "won't", "will not");
                    s0 = replaceAll(s0, "wanna", "want to");
                    s0 = replaceAll(s0, "gonna", "going to");
                    s0 = replaceAll(s0, "n't", " not");
                    s0 = replaceAll(s0, "'ve", " have");
                    s0 = replaceAll(s0, "'d", " had");
                    s0 = replaceAll(s0, "'ll", " will");
                    s0 = s0.replaceAll("[$\u20ac\u00a3\u00a5]+", CURR_SUBST);
                    s0 = s0.replaceAll("\\p{Punct}", "");
                    s0 = s0.replaceAll("[^\\p{ASCII}]", "");
                    s0 = replaceAll(s0, URL_SUBST, "[url]");
                    s0 = replaceAll(s0, CURR_SUBST, "[currency]");
                    s0 = replaceAll(s0, PERSON_SUBST, "[person]");
                    s0 = s0.replaceAll("[-.0-9]+", " [number] ");
                    s0 = s0.replaceAll("\\s+", " ");
                    StringTokenizer st = new StringTokenizer(s0);
                    boolean first = true;
                    StringBuilder sb = new StringBuilder();
                    while (st.hasMoreTokens()) {
                        if (!first)
                            sb.append(' ');
                        else
                            first = false;
                        sb.append(stemmer.stem(st.nextToken()));
                    }
                    s0 = sb.toString().trim();
                    if (s0.length() > 0) {
                        writer.write(s0);
                        writer.newLine();
                    }
                }
            }
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception e) {
                }
            }
            if (writer != null) {
                try {
                    writer.close();
                } catch (Exception e) {
                }
            }
        }
    }

    static class Models {
        SentenceModel sentenceModel = null;
        TokenizerModel tokenizerModel = null;
        TokenNameFinderModel tokenNameFinderModel = null;
    }
}
