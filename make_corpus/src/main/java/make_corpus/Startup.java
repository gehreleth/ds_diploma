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
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.*;
import java.util.*;
import java.util.regex.Pattern;

import static org.apache.commons.lang3.ArrayUtils.addAll;

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
        InputStream personFinderModelIn = null;
        InputStream tokenizerModelIn = null;
        InputStream posModelIn = null;
        InputStream orgNameFinderModelIn = null;
        InputStream timeFinderModelIn = null;
        InputStream dateFinderModelIn = null;
        InputStream locationModelIn = null;
        try {
            Models models = new Models();
            sentenceModelModelIn = new FileInputStream("en-sent.bin");
            models.sentenceModel = new SentenceModel(sentenceModelModelIn);
            tokenizerModelIn = new FileInputStream("en-token.bin");
            models.tokenizerModel = new TokenizerModel(tokenizerModelIn);
            personFinderModelIn = new FileInputStream("en-ner-person.bin");
            models.personNameFinderModel = new TokenNameFinderModel(personFinderModelIn);
            orgNameFinderModelIn = new FileInputStream("en-ner-organization.bin");
            models.organizationNameFinderModel = new TokenNameFinderModel(orgNameFinderModelIn);
            timeFinderModelIn = new FileInputStream("en-ner-time.bin");
            //models.timeFinderModel = new TokenNameFinderModel(timeFinderModelIn);
            dateFinderModelIn = new FileInputStream("en-ner-date.bin");
            models.dateFinderModel = new TokenNameFinderModel(dateFinderModelIn);
            locationModelIn = new FileInputStream("en-ner-location.bin");
            models.locationModel = new TokenNameFinderModel(locationModelIn);

            posModelIn = new FileInputStream("en-pos-maxent.bin");
            models.posModel = new POSModel(posModelIn);

            System.out.println("../src_data/en_US/en_US.blogs.txt");
            //models.vocabulary = GatherStats.buildVocabulary("../src_data/en_US/en_US.blogs.stats.db", NOUN_COUNT, VERB_COUNT, ADJ_COUNT);
            processSingleFile(models, "../src_data/en_US/en_US.blogs.txt", "../src_data/en_US/en_US.blogs.pp.txt");

            System.out.println("../src_data/en_US/en_US.news.txt");
           // models.vocabulary = GatherStats.buildVocabulary("../src_data/en_US/en_US.news.stats.db", NOUN_COUNT, VERB_COUNT, ADJ_COUNT);
            processSingleFile(models, "../src_data/en_US/en_US.news.txt", "../src_data/en_US/en_US.news.pp.txt");

           // models.vocabulary = GatherStats.buildVocabulary("../src_data/en_US/en_US.twitter.stats.db", NOUN_COUNT, VERB_COUNT, ADJ_COUNT);
            System.out.println("../src_data/en_US/en_US.twitter.txt");
            processSingleFile(models, "../src_data/en_US/en_US.twitter.txt", "../src_data/en_US/en_US.twitter.pp.txt");
        } finally {
            if (locationModelIn != null) {
                try {
                    locationModelIn.close();
                } catch (Exception e) {
                }
            }
            if (orgNameFinderModelIn != null) {
                try {
                    orgNameFinderModelIn.close();
                } catch (Exception e) {
                }
            }
            if (timeFinderModelIn != null) {
                try {
                    timeFinderModelIn.close();
                } catch (Exception e) {
                }
            }
            if (dateFinderModelIn != null) {
                try {
                    dateFinderModelIn.close();
                } catch (Exception e) {
                }
            }
            if (posModelIn != null) {
                try {
                    posModelIn.close();
                } catch (Exception e) {
                }
            }
            if (personFinderModelIn != null) {
                try {
                    personFinderModelIn.close();
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
        String lc = arg.toLowerCase();
        if (lc.indexOf(' ') == -1) {
            return !validWord(lc) || !vocabulary.contains(stemmer.stem(lc));
        } else {
            String[] lcs = lc.split("\\s");
            for (String lc0: lcs) {
                if (isSparse(vocabulary, stemmer, lc0))
                    return true;
            }
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
            NameFinderME personFinder = new NameFinderME(models.personNameFinderModel);
            NameFinderME orgFinder = new NameFinderME(models.organizationNameFinderModel);
            //NameFinderME timeFinder = new NameFinderME(models.timeFinderModel);
            NameFinderME dateFinder = new NameFinderME(models.dateFinderModel);
            NameFinderME locationFinder = new NameFinderME(models.locationModel);

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

                    Span[] personSpan = personFinder.find(sentence);
                    personFinder.clearAdaptiveData();

                    Span[] orgSpan = orgFinder.find(sentence);
                    orgFinder.clearAdaptiveData();

                   //Span[] timeSpan = timeFinder.find(sentence);
                    //timeFinder.clearAdaptiveData();

                    Span[] dateSpan = dateFinder.find(sentence);
                    dateFinder.clearAdaptiveData();

                    Span[] locationSpan = locationFinder.find(sentence);
                    locationFinder.clearAdaptiveData();

                    ArrayList<Span> templateSpans0 = new ArrayList<Span>();
                    templateSpans0.addAll(Arrays.asList(personSpan));
                    templateSpans0.addAll(Arrays.asList(orgSpan));
                    //templateSpans0.addAll(Arrays.asList(timeSpan));
                    templateSpans0.addAll(Arrays.asList(dateSpan));
                    templateSpans0.addAll(Arrays.asList(locationSpan));

                    Span[] templateSpans = templateSpans0.toArray(new Span[templateSpans0.size()]);

                    ArrayList<String> outToks = new ArrayList<String>();
                    int sparseCount = 0, nonSparseCount = 0;
                    for (int i = 0; i < sentence.length; i++) {
                        String token = sentence[i];
                        String prevToken = null;

                        {   int ix = outToks.size() - 1;
                            if (ix >= 0) {
                                prevToken = outToks.get(ix);
                            }
                        }

                        String posTag = postags[i];

                        boolean sparse = true;
                        boolean listItem = false;
                        boolean cardinal = false;

                        Span templateSpan = checkNameSpan(templateSpans, i);
                        if (templateSpan != null) {
                            i = templateSpan.getEnd() - 1;
                        } else {
                            token = token.replaceAll("\\p{Punct}", " ").replaceAll("\\s+", " ").trim();
                            if (token.length() == 0)
                                continue;
                        }

                        if (templateSpan == null) {
                            if ("s".equalsIgnoreCase(token)) {
                                if ("VBZ".equals(posTag)) {
                                    token = "is";
                                } else if (prevToken != null) {
                                    int ix = outToks.size() - 1;
                                    outToks.set(ix, prevToken + "'s");
                                    continue; // We aren't going to introduce a new token here, so let's skip iteration.
                                }
                            } else if ("n t".equalsIgnoreCase(token) && "RB".equals(posTag)) {
                                token = "not";
                                if (prevToken != null) {
                                    int ix = outToks.size() - 1;
                                    if ("ca".equalsIgnoreCase(prevToken)) {
                                        outToks.set(ix, "can");
                                    } else if ("ai".equalsIgnoreCase(prevToken)) {
                                        outToks.set(ix, prevToken + "n't");
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
                            } else if ("d".equalsIgnoreCase(token) && ("MD".equals(posTag) || "VBD".equals(posTag))) {
                                token = "would";
                            } else if ("u".equalsIgnoreCase(token) && "PRP".equals(posTag)) {
                                token = "you";
                            }

                            if (posTag.startsWith("N") || posTag.startsWith("V") || posTag.startsWith("J")){
                            //sparse = isSparse(models.vocabulary, stemmer, token);
                            } else {
                                listItem = posTag.startsWith("LS");
                                cardinal = posTag.startsWith("CD");
                                sparse = listItem || cardinal;
                            }
                        }

                        if (sparse) {
                            ++sparseCount;
                        } else {
                            ++nonSparseCount;
                        }

                        String res;
                        if (!sparse) {
                            res = token;
                        } else if (templateSpan != null) {
                            res = "#" + templateSpan.getType();
                        } else {
                            res = "#" + (listItem ? "listItem" : cardinal ? "number" : posTag);
                        }
                        if (prevToken == null || !(prevToken.startsWith("#") && prevToken.equals(res))) {
                            outToks.add(res);
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
