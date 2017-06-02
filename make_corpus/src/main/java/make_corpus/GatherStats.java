package make_corpus;

import opennlp.tools.namefind.NameFinderME;
import opennlp.tools.namefind.TokenNameFinderModel;
import opennlp.tools.postag.POSModel;
import opennlp.tools.postag.POSTaggerME;
import opennlp.tools.sentdetect.SentenceDetectorME;
import opennlp.tools.sentdetect.SentenceModel;
import opennlp.tools.tokenize.Tokenizer;
import opennlp.tools.tokenize.TokenizerME;
import opennlp.tools.tokenize.TokenizerModel;
import opennlp.tools.util.Span;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.*;
import java.sql.*;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by serge on 5/10/2017.
 */
public class GatherStats {

    public static final int MAX_NGRAM = 6;
    public static final Set<String> NOT_PART;
    public static final Set<String> PRPS;

    public static final Pattern TIME_PATTERN = Pattern.compile("(\\s+(?<hh>\\d{1,2})(:(?<mm>\\d{2}))?\\s*(?<ampm>[Aa]\\.?[Mm]\\.?|[Pp]\\.?[Mm])\\.?)|(\\s+(?<hhh>\\d{1,2}):(?<mmm>\\d{2}))");
    public static final Pattern DIGITS_PATTERN = Pattern.compile("\\d+");
    public static final String VERB_PREFIX = "V";
    public static final String NOUN_PREFIX = "N";
    public static final String ADJ_PREFIX = "J";
    public static final String APOSTOPHE_S = "'s";
    public static final Set<String> BASIC_NUMERICS;

    private static final String[] times = new String[]{"noon",
            "One o'clock",
            "Two o'clock",
            "Three o'clock",
            "Four o'clock",
            "Five o'clock",
            "Six o'clock",
            "Seven o'clock",
            "Eight o'clock",
            "Nine o'clock",
            "Ten o'clock",
            "Eleven o'clock",
            "Twelve o'clock"};

    private static ArrayList<ExecuteWithSqliteConn> BUFF = new ArrayList<ExecuteWithSqliteConn>();
    private static ExecuteWithSqliteConn take() throws InterruptedException {
        synchronized (BUFF) {
            ExecuteWithSqliteConn retVal = null;
            while (BUFF.isEmpty()) {
                BUFF.wait();
            }
            retVal = BUFF.remove(BUFF.size() - 1);
            BUFF.notifyAll();
            return retVal;
        }
    }

    private static void push(ExecuteWithSqliteConn task) throws InterruptedException {
        synchronized (BUFF) {
            while (BUFF.size() > 5000) {
                BUFF.wait();
            }
            BUFF.add(task);
            BUFF.notifyAll();
        }
    }

    static {
        try {
            Class.forName("org.sqlite.JDBC");
            //c = DriverManager.getConnection("jdbc:sqlite:test.db");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        HashSet<String> notPart = new HashSet<String>();
        notPart.addAll(Arrays.asList(
                "are",
                "ai",
                "can",
                "ca",
                "could",
                "dare",
                "did",
                "does",
                "do",
                "had",
                "has",
                "have",
                "is",
                "might",
                "must",
                "need",
                "ought",
                "should",
                "was",
                "wo",
                "were",
                "would"));
        NOT_PART = Collections.unmodifiableSet(notPart);
        HashSet<String> basicNemerics = new HashSet<String>();
        basicNemerics.addAll(Arrays.asList("zero",
                                "one",
                                "two",
                                "three",
                                "four",
                                "five",
                                "six",
                                "seven",
                                "eight",
                                "nine",
                                "ten",
                                "eleven",
                                "twelve",
                                "thirteen",
                                "fourteen",
                                "fifteen",
                                "sixteen",
                                "seventeen",
                                "eighteen",
                                "nineteen",
                                "twenty",
                                "thirty",
                                "forty",
                                "fifty",
                                "sixty",
                                "seventy",
                                "eighty",
                                "ninety",
                                "million",
                                "billion"));
        BASIC_NUMERICS = Collections.unmodifiableSet(basicNemerics);
        HashSet<String> prps = new HashSet<String>();
        prps.addAll(Arrays.asList("i", "you", "he", "she", "it", "you", "they"));
        PRPS = Collections.unmodifiableSet(prps);
    }

    private static final Pattern SQ_BRACES = Pattern.compile("\\[([^]]+)]");

    public static void main(String[] args) throws IOException, SQLException, InterruptedException {
        Connection conn0 = null;
        try {
            conn0 = initializeDatabase("../src_data/en_US/en_US.db");
            final Connection conn = conn0;
            Thread inserterThread = new Thread(new Runnable() {
                public void run() {
                    exit:
                    while (true) {
                        try {
                            ExecuteWithSqliteConn exec = take();
                            if (exec instanceof GatherStats.EndMarker) {
                                break exit;
                            }
                            exec.run(conn);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                    try {
                        conn.commit();
                        createIndices(conn);
                        aggregateRecords(conn);
                        dropTempTables(conn);
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }
            });

            final String[] vocabulary = buildVocabulary("../src_data/en_US/words.txt");

           Thread blogsParser = new Thread(new Runnable() {
                public void run() {
                    try {
                        threadMain("../src_data/en_US/en_US.blogs.txt", vocabulary);
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }
            });

            Thread newsParser = new Thread(new Runnable() {
                public void run() {
                    try {
                        threadMain("../src_data/en_US/en_US.news.txt", vocabulary);
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }
            });

            Thread twitterParser = new Thread(new Runnable() {
                public void run() {
                    try {
                        threadMain("../src_data/en_US/en_US.twitter.txt", vocabulary);
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }
            });
            blogsParser.start();
            newsParser.start();
            twitterParser.start();
            inserterThread.start();
            blogsParser.join();
            newsParser.join();
            twitterParser.join();
            push(new GatherStats.EndMarker());
            inserterThread.join();
        } finally {
            if (conn0 != null) {
                try {
                    conn0.close();
                } catch (Exception e) {
                }
            }
        }
    }

    public static void threadMain(String srcFile, String[] vocalulary) throws IOException, SQLException, InterruptedException {
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
            models.timeFinderModel = new TokenNameFinderModel(timeFinderModelIn);
            dateFinderModelIn = new FileInputStream("en-ner-date.bin");
            models.dateFinderModel = new TokenNameFinderModel(dateFinderModelIn);
            locationModelIn = new FileInputStream("en-ner-location.bin");
            models.locationModel = new TokenNameFinderModel(locationModelIn);
            models.vocabulary = vocalulary;

            posModelIn = new FileInputStream("en-pos-maxent.bin");
            models.posModel = new POSModel(posModelIn);

            processSingleFile(models, srcFile);
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

    private static String[] buildVocabulary(String fileName) throws IOException {
        ArrayList<String> acc = new ArrayList<String>();
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new InputStreamReader(new FileInputStream(new File(fileName))));
            while (true) {
                String line = reader.readLine();
                if (line == null)
                    break;
                acc.add(line);
            }
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception e) {
                }
            }
        }
        acc.add(":");
        acc.add("%");

        String[] retVal = acc.toArray(new String[acc.size()]);
        Arrays.sort(retVal, new Comparator<String>() {
            @Override
            public int compare(String o1, String o2) {
                return o1.compareTo(o2);
            }
        });
        return retVal;
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

    /*
I am 	I'm 	I'm (= I am) already here.

I have 	I've 	I've (= I have) seen that movie several times.
You have 	You've 	You've (= you have) been such a good friend to me.
They have 	They've 	I hear that they've (= they have) been told everything.
We have 	We've 	We've (= we have) tried to get a hold of you, but failed.

I will 	I'll 	I'll (= I will) deal with this.
You will 	You'll 	You'll (= you will) see him soon enough.
They will 	They'll 	I hope they'll (= they will) be on time.
We will 	We'll 	We'll (= we will) watch over the kids.
There will 	There'll 	They say there'll (= there will) be a new school in our district.

I had / I would 	I'd 	I'd (= I had) done it by the time you came.
I promised you I'd (= I would) do it.
You'd (= you would) like it, I'm sure.
You had / you would 	You'd 	You passed the test because you'd (= you had) prepared for it.
He will 	He'll 	He'll (= he will) show up, he is just running a little late.
He had / he would 	He'd 	He'd (= he had) helped me a lot to finish the work by your arrival.
He'd (= he would) be very glad to contribute.
They had / they would 	They'd 	They'd (= they had) done their work long before I started doing mine.
We'd (= we would) be much obliged if you helped us.
I talked to them and they promised they'd (= they would) do everything in their power.
There had / there would 	There'd 	There'd (= there had) been many people here before.
I knew there'd (= ther would) be a way.

You are 	You're 	You're (= you are) one of the best students in this class.
They are 	They're 	We're (= we are) going to talk about it next time.

He is / he has 	He's 	He's (= he is) a very talented actor.
He's (= he has) never lied to us.
She is / she has 	She's 	She's (= she is) standing by the window.
There is / there has 	There's 	There's (=there is) little time left.
There's (= there has) been a very nice chinese restaurant down the street before, but now it's gone.

She's (= she has) got a lot of money.
She will 	She'll 	She'll (= she will) come over to our house tonight.
She had / she would 	She'd 	She'd (= she had) called me before she came.

She said that she'd (= she would) give me a call during the lunch-break.
It is / it has 	It's 	It's (= it is) hot today.

It's (= it has) never been so hot.
We are 	We're 	We're (= we are) coming, we're almost there.
We had / we would 	We'd 	We'd (= we had) traveled from Germany to Spain.

Are not 	Aren't 	They aren't (= are not) here yet.
Cannot 	Can't 	I can't (= cannot) do it because I am very busy.
Could not 	Couldn't 	Why couldn't (= could not) you come in time?
Dare not 	Daren't 	I daren't (= dare not) say it.
Did not 	Didn't 	Helen says she didn't (= did not) know anything about it.
Does not 	Doesn't 	He doesn't (= does not) like this book.
Do not 	Don't 	Whatever you do, just don't (= do not) touch my antique statuettes.
Had not 	Hadn't 	We hadn't (= had not) seen such a beatiful place before we went there.
Has not 	Hasn't 	Sam hasn't (= has not) read that magazine yet, give it to him.
Have not 	Haven't 	I haven't (= have not) finished working yet, give me some more time.
Is not 	Isn't 	I don't know why he isn't (= is not) there.
Might not 	Mightn't 	You should call him first, he mightn't (= might not) be home yet.
Must not 	Mustn't 	You mustn't (= must not) work so hard, have a little rest.
Need not 	Needn't 	The teacher has said that we needn't (= need not) do this exercise.
Ought not 	Oughtn't 	Tell him that he oughtn't (= ought not) to speak with his parents like that.
Shall not 	Shan't 	Don't come tomorrow, I shan't (= shall not) be able to help you.
Should not 	Shouldn't 	We shouldn't (= should not) hurry, the work should be done very carefully.
Was not 	Wasn't 	I wasn't (= was not) ready to go when you called me.
Were not 	Weren't 	They weren't (= were not) going to come.
Will not 	Won't 	We won't (= will not) let you down.
Would not 	Wouldn't 	If I were you I wouldn't (= would not) underestimate him.
     */

    public static void processSingleFile(Models models, String inputFileName) throws IOException, SQLException, InterruptedException {
        BufferedReader reader = null;
        try {
            SentenceDetectorME sentenceDetector = new SentenceDetectorME(models.sentenceModel);
            OpennlpContext opennlpContext = new OpennlpContext();
            opennlpContext.tokenizer = new TokenizerME(models.tokenizerModel);
            opennlpContext.personFinder = new NameFinderME(models.personNameFinderModel);
            opennlpContext.orgFinder = new NameFinderME(models.organizationNameFinderModel);
            opennlpContext.timeFinder = new NameFinderME(models.timeFinderModel);
            opennlpContext.dateFinder = new NameFinderME(models.dateFinderModel);
            opennlpContext.locationFinder = new NameFinderME(models.locationModel);
            opennlpContext.tagger = new POSTaggerME(models.posModel);
            reader = new BufferedReader(new InputStreamReader(new FileInputStream(inputFileName)));
            int count = 0;
            while (true) {
                String line = reader.readLine();
                if (line == null)
                    break;
                String[] sentences = sentenceDetector.sentDetect(line);
                for (String rawSentence : sentences) {
                    System.out.printf("\r Sentence : %d", ++count);
                    processSentenceSubst(models, opennlpContext, true, rawSentence);
                    processSentenceSubst(models, opennlpContext, false, rawSentence);
                }
            }

       } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception e) {
                }
            }
        }
    }

    private static void processSentenceSubst(Models models, OpennlpContext opennlpContext, boolean createMacros, String rawSentence) throws InterruptedException, SQLException {
        String dbg = rawSentence;
        rawSentence = removeNonUnicodeChars(rawSentence);
        rawSentence = removeDots(rawSentence);


        ArrayList<Span> templateSpans0 = new ArrayList<Span>();
        String[] sentence = opennlpContext.tokenizer.tokenize(rawSentence);
        String postags[] = opennlpContext.tagger.tag(sentence);

        if (createMacros) {
            Span[] personSpan = opennlpContext.personFinder.find(sentence);
            opennlpContext.personFinder.clearAdaptiveData();

            Span[] orgSpan = opennlpContext.orgFinder.find(sentence);
            opennlpContext.orgFinder.clearAdaptiveData();

            Span[] dateSpan = opennlpContext.dateFinder.find(sentence);
            opennlpContext.dateFinder.clearAdaptiveData();

            Span[] timeSpan = opennlpContext.timeFinder.find(sentence);
            opennlpContext.timeFinder.clearAdaptiveData();

            Span[] locationSpan = opennlpContext.locationFinder.find(sentence);
            opennlpContext.locationFinder.clearAdaptiveData();

            templateSpans0.addAll(Arrays.asList(personSpan));
            templateSpans0.addAll(Arrays.asList(orgSpan));
            templateSpans0.addAll(Arrays.asList(dateSpan));
            templateSpans0.addAll(Arrays.asList(timeSpan));
            templateSpans0.addAll(Arrays.asList(locationSpan));
        }

        Span[] templateSpans = templateSpans0.toArray(new Span[templateSpans0.size()]);
        ArrayList<String> outToks = new ArrayList<String>();

        outToks.add("#b");
        for (int i = 0; i < sentence.length; i++) {
            String token = sentence[i];
            String prevToken = null;

            {   int ix = outToks.size() - 1;
                if (ix >= 0) {
                    prevToken = outToks.get(ix);
                }
            }

            String posTag = postags[i];

            Span templateSpan = checkNameSpan(templateSpans, i);
            if (templateSpan != null) {
                i = templateSpan.getEnd() - 1;
                token = StringUtils.join(ArrayUtils.subarray(sentence, templateSpan.getStart(), templateSpan.getEnd()), " ");
            }

            if (templateSpan == null) {
                token = replaceAll(token, ":", "abca668bd918a519226db7fa0ea0da01cff015cf");
                token = replaceAll(token, "%", "258e79facea2fd35bd92f9da3d922f1852b74190");
                token = replaceAll(token, "$", "d4f00bc54048c3281213673ef4b30d4a4afcb6b3");
                token = token.toLowerCase().replaceAll("\\p{Punct}", " ").replaceAll("\\s+", " ").trim();
                token = replaceAll(token, "abca668bd918a519226db7fa0ea0da01cff015cf", ":");
                token = replaceAll(token, "258e79facea2fd35bd92f9da3d922f1852b74190", "%");
                token = replaceAll(token, "d4f00bc54048c3281213673ef4b30d4a4afcb6b3", "$");
                if (StringUtils.isEmpty(token)) {
                    continue;
                }
                if ("s".equalsIgnoreCase(token)) {
                    int ix = outToks.size() - 1;
                    if ("VBZ".equals(posTag)) {
                        if (prevToken != null) {
                            if ("he".equalsIgnoreCase(prevToken)) {
                                outToks.set(ix, prevToken + APOSTOPHE_S);
                                continue;
                            } else if ("she".equalsIgnoreCase(prevToken)) {
                                outToks.set(ix, prevToken + APOSTOPHE_S);
                                continue;
                            } else if ("there".equalsIgnoreCase(prevToken)) {
                                outToks.set(ix, prevToken + APOSTOPHE_S);
                                continue;
                            } else if ("it".equalsIgnoreCase(prevToken)) {
                                outToks.set(ix, prevToken + APOSTOPHE_S);
                                continue;
                            } else if ("that".equalsIgnoreCase(prevToken)) {
                                outToks.set(ix, prevToken + APOSTOPHE_S);
                                continue;
                            } else {
                                token = "is";
                            }
                        } else {
                            token = "is";
                        }
                    } else if (prevToken != null) {
                        outToks.set(ix, prevToken + APOSTOPHE_S);
                        continue; // We aren't going to introduce a new token here, so let's skip iteration.
                    }
                } else if ("n t".equalsIgnoreCase(token) && "RB".equals(posTag)) {
                    if (prevToken != null) {
                        int ix = outToks.size() - 1;
                        if (NOT_PART.contains(prevToken.toLowerCase())) {
                            outToks.set(ix, prevToken + "n't");
                            continue; // We aren't going to introduce a new token here, so let's skip iteration.
                        }
                    } else {
                        token = "not";
                    }
                } else if ("ve".equalsIgnoreCase(token) && posTag.startsWith(VERB_PREFIX)) {
                    int ix = outToks.size() - 1;
                    if (prevToken != null) {
                        if (PRPS.contains(prevToken.toLowerCase())) {
                            outToks.set(ix, prevToken + "'ve");
                            continue;
                        } else {
                            token = "have";
                        }
                    } else {
                        token = "have";
                    }
                } else if ("re".equalsIgnoreCase(token) && posTag.startsWith(VERB_PREFIX)) {
                    int ix = outToks.size() - 1;
                    if (prevToken != null) {
                        if ("you".equalsIgnoreCase(prevToken)) {
                            outToks.set(ix, prevToken + "'re");
                            continue;
                        } else if ("they".equalsIgnoreCase(prevToken)) {
                            outToks.set(ix, prevToken + "'re");
                            continue;
                        } else if ("we".equalsIgnoreCase(prevToken)) {
                            outToks.set(ix, prevToken + "'re");
                            continue;
                        } else {
                            token = "are";
                        }
                    } else {
                        token = "are";
                    }
                } else if ("m".equalsIgnoreCase(token) && posTag.startsWith(VERB_PREFIX)) {
                    int ix = outToks.size() - 1;
                    if (prevToken != null) {
                        if ("i".equalsIgnoreCase(prevToken)) {
                            outToks.set(ix, prevToken + "'m");
                            continue;
                        } else {
                            token = "am";
                        }
                    } else {
                        token = "am";
                    }
                } else if ("ll".equalsIgnoreCase(token) && "MD".equals(posTag)) {
                    int ix = outToks.size() - 1;
                    if (prevToken != null) {
                        if (PRPS.contains(prevToken.toLowerCase())) {
                            outToks.set(ix, prevToken + "'ll");
                            continue;
                        } else {
                            token = "will";
                        }
                    } else {
                        token = "will";
                    }
                } else if ("d".equalsIgnoreCase(token) && ("MD".equals(posTag) || "VBD".equals(posTag))) {
                    int ix = outToks.size() - 1;
                    if (prevToken != null) {
                        if (PRPS.contains(prevToken.toLowerCase())) {
                            outToks.set(ix, prevToken + "'d");
                            continue;
                        } else {
                            token = "MD".equals(posTag) ? "would" : "had";
                        }
                    } else {
                        token = "MD".equals(posTag) ? "would" : "had";
                    }
                } else if ("u".equalsIgnoreCase(token) && "PRP".equals(posTag)) {
                    token = "you";
                } else if ("o clock".equals(token)) {
                    token = "o'clock";
                }
                if (posTag.startsWith(NOUN_PREFIX) || posTag.startsWith(VERB_PREFIX) || posTag.startsWith(ADJ_PREFIX)) {
                    if (token.indexOf(' ') == -1) {
                        addToken(outToks, vocabularyFilter(models.vocabulary, token));
                    } else {
                        String[] lcs = token.split("\\s");
                        for (String lc0 : lcs) {
                            addToken(outToks, vocabularyFilter(models.vocabulary, lc0));
                        }
                    }
                } else if (posTag.startsWith("LS") || posTag.startsWith("CD")) {
                    if (!BASIC_NUMERICS.contains(token)) {
                        addToken(outToks, "#number");
                        updateMacroStats("number", token);
                    } else {
                        addToken(outToks, token);
                    }
                } else {
                    Matcher m = DIGITS_PATTERN.matcher(token);
                    if (!m.matches()) {
                        addToken(outToks, token);
                    } else {
                        addToken(outToks,"#number");
                        updateMacroStats("number", token);
                    }
                }
            } else {
                updateMacroStats(templateSpan.getType(), token);
                addToken(outToks,"#" + templateSpan.getType());
            }
        }
        boolean containsNonDictWord = false;
        int numberOfDictWords = 0;
        for (String str : outToks) {
            if (!str.startsWith("#")) {
                ++numberOfDictWords;
            }
        }
        if (!containsNonDictWord && numberOfDictWords > 3) {
            storeNgrams(Collections.unmodifiableList(outToks));
        }
    }

    private static String removeDots(String rawSentence) {
        rawSentence = rawSentence.replaceAll("\\.", " ");
        rawSentence = rawSentence.replaceAll("\\s+[Aa]\\s+[Mm]\\s+", " am ");
        rawSentence = rawSentence.replaceAll("\\s+[Pp]\\s+[Mm]\\s+", " pm ");
        rawSentence = rawSentence.replaceAll("\\s+[Uu]\\s+[Ss]\\s+", " United States ");
        rawSentence = rawSentence.replaceAll("\\s+[Nn]\\s+[Yy]\\s+", " New York ");
        rawSentence = rawSentence.replaceAll("\\s+[Nn]\\s+[Jj]\\s+", " New Jersey ");
        rawSentence = rawSentence.replaceAll("\\s+[Pp][Hh]\\s+[Dd]\\s+", " PhD ");
        rawSentence = replaceAll(rawSentence,":", " : ");
        rawSentence = replaceAll(rawSentence,"%", " % ");
        rawSentence = replaceAll(rawSentence,"$", " $ ");
        return rawSentence;
    }

    private static String removeNonUnicodeChars(String rawSentence) {
        rawSentence = replaceAll(rawSentence, "\u201c", "\"");
        rawSentence = replaceAll(rawSentence, "\u201d", "\"");
        rawSentence = replaceAll(rawSentence, "\u2019", "\'");
        rawSentence = replaceAll(rawSentence,"`", "\'");
        rawSentence = rawSentence.replaceAll("n'\\s+t", "n\'t");

        rawSentence = rawSentence.replaceAll("[^\\p{ASCII}]", "");
        return rawSentence;
    }

    private static String vocabularyFilter(String[] vocabulary, String token) throws InterruptedException {
        if (Arrays.binarySearch(vocabulary, token) >= 0)
            return token;
        else {
            updateMacroStats("unk", token);
            return "#unk";
        }
    }

    private static void updateMacroStats(final String type, final String text) throws InterruptedException {
        push(new ExecuteWithSqliteConn() {
            @Override
            public void run(Connection conn) throws SQLException{
                PreparedStatement stmt = null;
                String sql;
                try {
                    sql = "insert into " + type + "_tmp(name) values(?)";
                    stmt = conn.prepareStatement(sql);
                    stmt.setString(1, text);
                    stmt.executeUpdate();
                } finally {
                    if (stmt != null) try {
                        stmt.close();
                    } catch (Exception e) {
                    }
                }
            }
        });
    }

    private static void dropTempTables(Connection conn) throws SQLException {
        Statement stmt = null;
        try {
            stmt = conn.createStatement();
            System.out.println("Drop n1gram_tmp...");
            stmt.executeUpdate("DROP TABLE n1gram_tmp");
            System.out.println("Drop n2gram_tmp...");
            stmt.executeUpdate("DROP TABLE n2gram_tmp");
            System.out.println("Drop n3gram_tmp...");
            stmt.executeUpdate("DROP TABLE n3gram_tmp");
            System.out.println("Drop n4gram_tmp...");
            stmt.executeUpdate("DROP TABLE n4gram_tmp");
            System.out.println("Drop n5gram_tmp...");
            stmt.executeUpdate("DROP TABLE n5gram_tmp");
            System.out.println("Drop n6gram_tmp...");
            stmt.executeUpdate("DROP TABLE n6gram_tmp");
            System.out.println("Drop person_tmp...");
            stmt.executeUpdate("DROP TABLE person_tmp");
            System.out.println("Drop number_tmp...");
            stmt.executeUpdate("DROP TABLE number_tmp");
            System.out.println("Drop organization_tmp...");
            stmt.executeUpdate("DROP TABLE organization_tmp");
            System.out.println("Drop location_tmp...");
            stmt.executeUpdate("DROP TABLE location_tmp");
            System.out.println("Drop time_tmp...");
            stmt.executeUpdate("DROP TABLE time_tmp");
            System.out.println("Drop date_tmp...");
            stmt.executeUpdate("DROP TABLE date_tmp");
            System.out.println("Drop unk_tmp...");
            stmt.executeUpdate("DROP TABLE unk_tmp");
            conn.commit();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void aggregateRecords(Connection conn) throws SQLException {
        Statement stmt = null;
        try {
            stmt = conn.createStatement();
            System.out.println("Aggregate n1gram ...");
            stmt.executeUpdate("CREATE TABLE n1gram AS SELECT w1, count(1) - 2 as `count` FROM n1gram_tmp GROUP BY w1 HAVING `count` > 0");
            System.out.println("Aggregate n2gram ...");
            stmt.executeUpdate("CREATE TABLE n2gram AS SELECT w1, w2, count(1) - 2 as `count` FROM n2gram_tmp GROUP BY w1, w2  HAVING `count` > 0");
            System.out.println("Aggregate n3gram ...");
            stmt.executeUpdate("CREATE TABLE n3gram AS SELECT w1, w2, w3, count(1) - 2 as `count` FROM n3gram_tmp GROUP BY w1, w2, w3 HAVING `count` > 0");
            System.out.println("Aggregate n4gram ...");
            stmt.executeUpdate("CREATE TABLE n4gram AS SELECT w1, w2, w3, w4, count(1) - 2 as `count` FROM n4gram_tmp GROUP BY w1, w2, w3, w4 HAVING `count` > 0");
            System.out.println("Aggregate n5gram ...");
            stmt.executeUpdate("CREATE TABLE n5gram AS SELECT w1, w2, w3, w4, w5, count(1) - 2 as `count` FROM n5gram_tmp GROUP BY w1, w2, w3, w4, w5 HAVING `count` > 0");
            System.out.println("Aggregate n6gram ...");
            stmt.executeUpdate("CREATE TABLE n6gram AS SELECT w1, w2, w3, w4, w5, w6, count(1) - 2 as `count` FROM n6gram_tmp GROUP BY w1, w2, w3, w4, w5, w6 HAVING `count` > 0");
            System.out.println("Aggregate person ...");
            stmt.executeUpdate("CREATE TABLE person AS SELECT name, count(1) - 1 as `count` FROM person_tmp GROUP BY name HAVING `count` > 0");
            System.out.println("Aggregate number ...");
            stmt.executeUpdate("CREATE TABLE number AS SELECT name, count(1) - 1 as `count` FROM number_tmp GROUP BY name HAVING `count` > 0");
            System.out.println("Aggregate organization ...");
            stmt.executeUpdate("CREATE TABLE organization AS SELECT name, count(1) - 1 as `count` FROM organization_tmp GROUP BY name HAVING `count` > 0");
            System.out.println("Aggregate location ...");
            stmt.executeUpdate("CREATE TABLE location AS SELECT name, count(1) - 1 as `count` FROM location_tmp GROUP BY name HAVING `count` > 0");
            System.out.println("Aggregate time ...");
            stmt.executeUpdate("CREATE TABLE time AS SELECT name, count(1) - 1 as `count` FROM time_tmp GROUP BY name HAVING `count` > 0");
            System.out.println("Aggregate date ...");
            stmt.executeUpdate("CREATE TABLE date AS SELECT name, count(1) - 1 as `count` FROM date_tmp GROUP BY name HAVING `count` > 0");
            System.out.println("Aggregate unk ...");
            stmt.executeUpdate("CREATE TABLE unk AS SELECT name, count(1) - 1 as `count` FROM unk_tmp GROUP BY name HAVING `count` > 0");
            conn.commit();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void createIndices(Connection conn) throws SQLException {
        Statement stmt = null;
        try {
            stmt = conn.createStatement();
            System.out.println("Create index on n1gram");
            stmt.executeUpdate("CREATE INDEX n1gram_ix0 ON n1gram_tmp(w1)");
            System.out.println("Create index on n2gram");
            stmt.executeUpdate("CREATE INDEX n2gram_ix0 ON n2gram_tmp(w1, w2)");
            System.out.println("Create index on n3gram");
            stmt.executeUpdate("CREATE INDEX n3gram_ix0 ON n3gram_tmp(w1, w2, w3)");
            System.out.println("Create index on n4gram");
            stmt.executeUpdate("CREATE INDEX n4gram_ix0 ON n4gram_tmp(w1, w2, w3, w4)");
            System.out.println("Create index on n5gram");
            stmt.executeUpdate("CREATE INDEX n5gram_ix0 ON n5gram_tmp(w1, w2, w3, w4, w5)");
            System.out.println("Create index on n6gram");
            stmt.executeUpdate("CREATE INDEX n6gram_ix0 ON n6gram_tmp(w1, w2, w3, w4, w5, w6)");
            System.out.println("Create index on person");
            stmt.executeUpdate("CREATE INDEX person_ix0 ON person_tmp(name)");
            System.out.println("Create index on location");
            stmt.executeUpdate("CREATE INDEX location_ix0 ON location_tmp(name)");
            System.out.println("Create index on organization");
            stmt.executeUpdate("CREATE INDEX organization_ix0 ON organization_tmp(name)");
            System.out.println("Create index on time");
            stmt.executeUpdate("CREATE INDEX time_ix0 ON time_tmp(name)");
            System.out.println("Create index on date");
            stmt.executeUpdate("CREATE INDEX date_ix0 ON date_tmp(name)");
            System.out.println("Create index on number");
            stmt.executeUpdate("CREATE INDEX number_ix0 ON number_tmp(name)");
            System.out.println("Create index on unk");
            stmt.executeUpdate("CREATE INDEX unk_ix0 ON unk_tmp(name)");
            conn.commit();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void storeNgrams(final List<String> outToks)
            throws SQLException, InterruptedException {
        push(new ExecuteWithSqliteConn() {
            @Override
            public void run(Connection conn) throws SQLException {
                String[] ngrams = new String[MAX_NGRAM];
                for (String tok : outToks) {
                    for (int i = 0; i < MAX_NGRAM - 1; i++) {
                        ngrams[i] = ngrams[i + 1];
                    }
                    ngrams[MAX_NGRAM - 1] = tok;
                    updateN1Gram(conn, ngrams);
                    updateN2Gram(conn, ngrams);
                    updateN3Gram(conn, ngrams);
                    updateN4Gram(conn, ngrams);
                    updateN5Gram(conn, ngrams);
                    updateN6Gram(conn, ngrams);
                }
            }
        });
    }

    private static void updateN1Gram(Connection conn, String[] ngrams) throws SQLException {
        String w1 = ngrams[MAX_NGRAM - 1];
        PreparedStatement stmt = null;
        String sql;
        try {
            sql = "insert into n1gram_tmp(w1) values(?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, w1);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void updateN2Gram(Connection conn, String[] ngrams) throws SQLException {
        String w1 = ngrams[MAX_NGRAM - 2];
        String w2 = ngrams[MAX_NGRAM - 1];
        if (w1 == null || w2 == null)
            return;

        PreparedStatement stmt = null;
        String sql;
        try {
            sql = "insert into n2gram_tmp(w1, w2) values(?, ?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, w1);
            stmt.setString(2, w2);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void updateN3Gram(Connection conn, String[] ngrams) throws SQLException {
        String w1 = ngrams[MAX_NGRAM - 3];
        String w2 = ngrams[MAX_NGRAM - 2];
        String w3 = ngrams[MAX_NGRAM - 1];

        if (w1 == null || w2 == null || w3 == null)
            return;

        PreparedStatement stmt = null;
        String sql;
        try {
            sql = "insert into n3gram_tmp(w1, w2, w3) values(?, ?, ?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, w1);
            stmt.setString(2, w2);
            stmt.setString(3, w3);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void updateN4Gram(Connection conn, String[] ngrams) throws SQLException {
        String w1 = ngrams[MAX_NGRAM - 4];
        String w2 = ngrams[MAX_NGRAM - 3];
        String w3 = ngrams[MAX_NGRAM - 2];
        String w4 = ngrams[MAX_NGRAM - 1];

        if (w1 == null || w2 == null || w3 == null || w4 == null)
            return;

        PreparedStatement stmt = null;
        String sql;
        try {
            sql = "insert into n4gram_tmp(w1, w2, w3, w4) values(?, ?, ?, ?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, w1);
            stmt.setString(2, w2);
            stmt.setString(3, w3);
            stmt.setString(4, w4);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void updateN5Gram(Connection conn, String[] ngrams) throws SQLException {
        String w1 = ngrams[MAX_NGRAM - 5];
        String w2 = ngrams[MAX_NGRAM - 4];
        String w3 = ngrams[MAX_NGRAM - 3];
        String w4 = ngrams[MAX_NGRAM - 2];
        String w5 = ngrams[MAX_NGRAM - 1];

        if (w1 == null || w2 == null || w3 == null || w4 == null || w5 == null)
            return;

        PreparedStatement stmt = null;
        String sql;
        try {
            sql = "insert into n5gram_tmp(w1, w2, w3, w4, w5) values(?, ?, ?, ?, ?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, w1);
            stmt.setString(2, w2);
            stmt.setString(3, w3);
            stmt.setString(4, w4);
            stmt.setString(5, w5);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void updateN6Gram(Connection conn, String[] ngrams) throws SQLException {
        String w1 = ngrams[MAX_NGRAM - 6];
        String w2 = ngrams[MAX_NGRAM - 5];
        String w3 = ngrams[MAX_NGRAM - 4];
        String w4 = ngrams[MAX_NGRAM - 3];
        String w5 = ngrams[MAX_NGRAM - 2];
        String w6 = ngrams[MAX_NGRAM - 1];

        if (w1 == null || w2 == null || w3 == null || w4 == null || w5 == null || w6 == null)
            return;

        PreparedStatement stmt = null;
        String sql;
        try {
            sql = "insert into n6gram_tmp(w1, w2, w3, w4, w5, w6) values(?, ?, ?, ?, ?, ?)";
            stmt = conn.prepareStatement(sql);
            stmt.setString(1, w1);
            stmt.setString(2, w2);
            stmt.setString(3, w3);
            stmt.setString(4, w4);
            stmt.setString(5, w5);
            stmt.setString(6, w6);
            stmt.executeUpdate();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
    }

    private static void addToken(ArrayList<String> tokens, String token) {
        if ((token.length() > 0) && (tokens.isEmpty() || !token.equals(tokens.get(tokens.size() - 1)))) {
            tokens.add(token);
        }
    }

    private static ArrayList<String> getPosList(Connection conn, String pos, int count) throws SQLException {
        ArrayList<String> retVal = new ArrayList<String>();
        PreparedStatement stmt = null;
        try {
            String sql = "select stem from vocabulary where posTag like '" + pos + "%' and count > 1 order by count desc limit ?";
            stmt = conn.prepareStatement(sql);
            stmt.setInt(1, count);
            ResultSet rs = stmt.executeQuery();
            while (rs.next()) {
                retVal.add(rs.getString("stem"));
            }
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
        return retVal;
    }

    private static Connection openDatabase(String fileName) throws SQLException {
        File outputFile = new File(fileName);
        String url = "jdbc:sqlite://" + outputFile.getAbsoluteFile();
        Connection conn = DriverManager.getConnection(url);
        return conn;
    }

    private static Connection initializeDatabase(String outputFileName) throws SQLException {
        File outputFile = new File(outputFileName);
        if (outputFile.exists()) {
            outputFile.renameTo(new File(outputFileName + "." + System.currentTimeMillis()));
        }
        Connection conn = null;
        Statement stmt = null;
        String sql;
        try {
            String url = "jdbc:sqlite://" + outputFile.getAbsoluteFile();
            conn = DriverManager.getConnection(url);
            conn.setAutoCommit(false);

            stmt = conn.createStatement();
            sql = "CREATE TABLE n1gram_tmp(" +
                    " w1        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n2gram_tmp(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n3gram_tmp(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL, " +
                    " w3        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n4gram_tmp(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL, " +
                    " w3        TEXT  NOT NULL," +
                    " w4        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n5gram_tmp(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL, " +
                    " w3        TEXT  NOT NULL," +
                    " w4        TEXT  NOT NULL," +
                    " w5        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE n6gram_tmp(" +
                    " w1        TEXT  NOT NULL," +
                    " w2        TEXT  NOT NULL, " +
                    " w3        TEXT  NOT NULL," +
                    " w4        TEXT  NOT NULL," +
                    " w5        TEXT  NOT NULL," +
                    " w6        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE person_tmp(name              TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE organization_tmp(name        TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE time_tmp(name                TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE date_tmp(name                TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE location_tmp(name            TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE number_tmp(name              TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            sql = "CREATE TABLE unk_tmp(name              TEXT  NOT NULL)";
            stmt.executeUpdate(sql);

            conn.commit();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
        return conn;
    }

    private static class EndMarker implements ExecuteWithSqliteConn {
        @Override
        public void run(Connection conn) { }
    }

    private static class OpennlpContext {
        SentenceDetectorME sentenceDetector = null;
        Tokenizer tokenizer = null;
        NameFinderME personFinder = null;
        NameFinderME orgFinder = null;
        NameFinderME timeFinder = null;
        NameFinderME dateFinder = null;
        NameFinderME locationFinder = null;
        POSTaggerME tagger = null;
    }
}
