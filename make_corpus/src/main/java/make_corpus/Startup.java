package make_corpus;

import opennlp.tools.namefind.NameFinderME;
import opennlp.tools.namefind.TokenNameFinderModel;
import opennlp.tools.postag.POSModel;
import opennlp.tools.postag.POSTaggerME;
import opennlp.tools.sentdetect.SentenceDetectorME;
import opennlp.tools.sentdetect.SentenceModel;
import opennlp.tools.stemmer.PorterStemmer;
import opennlp.tools.tokenize.Tokenizer;
import opennlp.tools.tokenize.TokenizerME;
import opennlp.tools.tokenize.TokenizerModel;
import opennlp.tools.util.Span;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;

import java.io.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Startup {

    public static final String URL_SUBST = "5446fb31b66c149c85a381b19c94344a90f6574c";
    public static final String CURR_SUBST = "90540a48b75388cd11d3c82714e1444ad4c84268";
    public static final String PERSON_SUBST = "712c71ac36a11b2f8bdf0d1e3e360ed4d578e9c9";
    private static final Pattern SQ_BRACES = Pattern.compile("\\[([^]]+)]");

    private static final int NOUN_COUNT = 50000;
    private static final int VERB_COUNT = 50000;
    private static final int ADJ_COUNT = 50000;

    public static void main(String[] args) throws IOException {
        InputStream sentenceModelModelIn = null;
        InputStream tokenNameFinderModelIn = null;
        InputStream tokenizerModelIn = null;
        InputStream posModelIn = null;
        try {
            Models models = new Models();
            sentenceModelModelIn = new FileInputStream("en-sent.bin");
            models.sentenceModel = new SentenceModel(sentenceModelModelIn);
            tokenizerModelIn = new FileInputStream("en-token.bin");
            models.tokenizerModel = new TokenizerModel(tokenizerModelIn);
            tokenNameFinderModelIn = new FileInputStream("en-ner-person.bin");
            models.tokenNameFinderModel = new TokenNameFinderModel(tokenNameFinderModelIn);
            posModelIn = new FileInputStream("en-pos-maxent.bin");
            models.posModel = new POSModel(posModelIn);

            System.out.println("../src_data/en_US/en_US.blogs.txt");
            models.vocabulary = GatherStats.buildVocabulary("../src_data/en_US/en_US.blogs.stats.db", NOUN_COUNT, VERB_COUNT, ADJ_COUNT);
            processSingleFile(models, "../src_data/en_US/en_US.blogs.txt", "../src_data/en_US/en_US.blogs.pp.txt");

            System.out.println("../src_data/en_US/en_US.news.txt");
            models.vocabulary = GatherStats.buildVocabulary("../src_data/en_US/en_US.news.stats.db", NOUN_COUNT, VERB_COUNT, ADJ_COUNT);
            processSingleFile(models, "../src_data/en_US/en_US.news.txt", "../src_data/en_US/en_US.news.pp.txt");

            models.vocabulary = GatherStats.buildVocabulary("../src_data/en_US/en_US.twitter.stats.db", NOUN_COUNT, VERB_COUNT, ADJ_COUNT);
            System.out.println("../src_data/en_US/en_US.twitter.txt");
            processSingleFile(models, "../src_data/en_US/en_US.twitter.txt", "../src_data/en_US/en_US.twitter.pp.txt");
        } finally {
            if (posModelIn != null) {
                try {
                    posModelIn.close();
                } catch (Exception e) {
                }
            }
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

    static boolean validWord(String arg) {
        return arg.length() > 1 || arg.equals("i") || arg.equals("a") || arg.equals("s") || arg.equals("u");
    }

    public static boolean isSparse(HashSet<String> vocabulary, PorterStemmer stemmer, String arg) {
        if (!arg.startsWith("[")) {
            String lc = arg.toLowerCase();
            return !validWord(lc) || !vocabulary.contains(stemmer.stem(lc));
        } else {
            return false;
        }
    }

    private static Span checkNameSpan(Span[] namespans, int tokenNo) {
        if (namespans == null)
            return null;

        for (Span namespan : namespans) {
            if (namespan.contains(tokenNo))
                return namespan;
        }

        return null;
    }

    /*
    1.	CC	Coordinating conjunction
	2.	CD	Cardinal number
	3.	DT	Determiner
	4.	EX	Existential there
	5.	FW	Foreign word
	6.	IN	Preposition or subordinating conjunction
	7.	JJ	Adjective
	8.	JJR	Adjective, comparative
	9.	JJS	Adjective, superlative
	10.	LS	List item marker
	11.	MD	Modal
	12.	NN	Noun, singular or mass
	13.	NNS	Noun, plural
	14.	NNP	Proper noun, singular
	15.	NNPS	Proper noun, plural
	16.	PDT	Predeterminer
	17.	POS	Possessive ending
	18.	PRP	Personal pronoun
	19.	PRP$	Possessive pronoun
	20.	RB	Adverb
	21.	RBR	Adverb, comparative
	22.	RBS	Adverb, superlative
	23.	RP	Particle
	24.	SYM	Symbol
	25.	TO	to
	26.	UH	Interjection
	27.	VB	Verb, base form
	28.	VBD	Verb, past tense
	29.	VBG	Verb, gerund or present participle
	30.	VBN	Verb, past participle
	31.	VBP	Verb, non-3rd person singular present
	32.	VBZ	Verb, 3rd person singular present
	33.	WDT	Wh-determiner
	34.	WP	Wh-pronoun
	35.	WP$	Possessive wh-pronoun
	36.	WRB	Wh-adverb
     */
    public static void processSingleFile(Models models, String inputFileName, String outputFileName) throws IOException {
        PorterStemmer stemmer = new PorterStemmer();
        BufferedReader reader = null;
        BufferedWriter writer = null;
        try {
            SentenceDetectorME sentenceDetector = new SentenceDetectorME(models.sentenceModel);
            Tokenizer tokenizer = new TokenizerME(models.tokenizerModel);
            NameFinderME nameFinder = new NameFinderME(models.tokenNameFinderModel);
            POSTaggerME tagger = new POSTaggerME(models.posModel);

            reader = new BufferedReader(new InputStreamReader(new FileInputStream(inputFileName)));
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFileName)));
            int count = 0;
            while (true) {
                String line = reader.readLine();
                if (line == null)
                    break;
                String[] sentences = sentenceDetector.sentDetect(line);
                for (String rawSentence : sentences) {
                    System.out.printf("\r Sentence : %d", ++count);
                    rawSentence = replaceAll(rawSentence, "\u201c", "\"");
                    rawSentence = replaceAll(rawSentence, "\u201d", "\"");
                    rawSentence = replaceAll(rawSentence, "\u2019", "\'");
                    rawSentence = replaceAll(rawSentence, "wanna", "want to");
                    rawSentence = replaceAll(rawSentence, "gonna", "going to");

                    rawSentence = rawSentence.replaceAll("[^\\p{ASCII}]", "");

                    String[] sentence = tokenizer.tokenize(rawSentence);
                    String postags[] = tagger.tag(sentence);
                    Span[] namespans = nameFinder.find(sentence);

                    nameFinder.clearAdaptiveData();
                    ArrayList<String> outToks = new ArrayList<String>();
                    int sparseCount = 0, nonSparseCount = 0;
                    for (int i = 0; i < sentence.length; i++) {
                        String token = sentence[i].replaceAll("\\p{Punct}", "");
                        if (token.length() == 0)
                            continue;

                        String posTag = postags[i];
                        if ("s".equalsIgnoreCase(token)) {
                            if ("VBZ".equals(posTag)) {
                                token = "is";
                            } else if (!outToks.isEmpty()) {
                                int ix = outToks.size() - 1;
                                String oldToken = outToks.get(ix);
                                outToks.set(ix, oldToken + "'s");
                                continue; // We aren't going to introduce a new token here, so let's skip iteration.
                            }
                        } else if ("nt".equalsIgnoreCase(token) && "RB".equals(posTag)) {
                            token = "not";
                            if (!outToks.isEmpty()) {
                                int ix = outToks.size() - 1;
                                String oldToken = outToks.get(ix);
                                if ("ca".equalsIgnoreCase(oldToken)) {
                                    outToks.set(ix, "can");
                                } else if ("ai".equalsIgnoreCase(oldToken)) {
                                    outToks.set(ix, oldToken + "n't");
                                    continue; // We aren't going to introduce a new token here, so let's skip iteration.
                                }
                            }
                        } else if ("ve".equalsIgnoreCase(token) && "VBP".equals(posTag)) {
                            token = "have";
                        } else if ("re".equalsIgnoreCase(token) && "VBP".equals(posTag)) {
                            token = "are";
                        } else if ("m".equalsIgnoreCase(token) && "VBP".equals(posTag)) {
                            token = "am";
                        } else if ("ll".equalsIgnoreCase(token) && "MD".equals(posTag)) {
                            token = "will";
                        } else if ("d".equalsIgnoreCase(token) && "MD".equals(posTag)) {
                            token = "would";
                        } else if ("u".equalsIgnoreCase(token) && "PRP".equals(posTag)) {
                            token = "you";
                        }

                        boolean person = false;
                        boolean sparse = true;

                        Span nameSpan = checkNameSpan(namespans, i);
                        if (nameSpan != null) {
                            i = nameSpan.getEnd() - 1;
                            person = true;
                        } else {
                            if (posTag.startsWith("N") || posTag.startsWith("V") || posTag.startsWith("J"))
                                sparse = isSparse(models.vocabulary, stemmer, token);
                            else if (!(posTag.startsWith("LS") || posTag.startsWith("CD")))
                                sparse = false;
                        }

                        if (sparse) {
                            ++sparseCount;
                        } else {
                            ++nonSparseCount;
                        }

                        if (!sparse) {
                            outToks.add(token);
                        } else if (person) {
                            outToks.add("X_Person");
                        } else {
                            outToks.add("X_" + posTag);
                        }
                    }

                    if (nonSparseCount > 3 && nonSparseCount > sparseCount) {
                        String outStr = StringUtils.join(outToks, " ");
                        writer.write(outStr);
                        writer.write(".");
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
}
