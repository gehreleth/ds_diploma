package make_corpus;

import opennlp.tools.sentdetect.SentenceDetectorME;
import opennlp.tools.sentdetect.SentenceModel;
import opennlp.tools.stemmer.PorterStemmer;

import java.io.*;
import java.util.StringTokenizer;
import java.util.regex.*;

public class Startup
{
    public static void main( String[] args ) throws IOException {
        if (args.length < 2) {
            System.err.println("Usage make_corpus <raw corpus> <preprocessed corpus>");
            System.exit(1);
        }
        BufferedReader reader = null;
        BufferedWriter writer = null;
        InputStream modelIn = null;
        try {
            modelIn =  new FileInputStream("en-sent.bin");
            SentenceModel model = new SentenceModel(modelIn);
            SentenceDetectorME sentenceDetector = new SentenceDetectorME(model);

            reader = new BufferedReader(new InputStreamReader(new FileInputStream(args[0])));
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1])));
            PorterStemmer stemmer = new PorterStemmer();
            while (true) {
                String line = reader.readLine();
                if (line == null)
                    break;
                String[] sentences  = sentenceDetector.sentDetect(line);
                for (final String sentence : sentences) {
                    String sentence0 = sentence
                            .replaceAll("https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)", "url")
                            .replaceAll("['’]m", " am")
                            .replaceAll("['’]re", " are")
                            .replaceAll("['’]s", " is")
                            .replaceAll("can['’]t", "can not")
                            .replaceAll("won['’]t", "will not")
                            .replaceAll("n['’]t", " not")
                            .replaceAll("['’]ve", " have")
                            .replaceAll("['’]d", " had")
                            .replaceAll("['’]ll", " will")
                            .replaceAll("\\p{Punct}", "")
                            .replaceAll("[$£₤€]+", " [currency] ")
                            .replaceAll("[-.0-9]+", " [number] ")
                            .replaceAll("\\s+", " ");
                    StringTokenizer st = new StringTokenizer(sentence0);
                    boolean first = true;
                    while (st.hasMoreTokens()) {
                        if (!first)
                            writer.write(' ');
                        else
                            first = false;
                        writer.write(stemmer.stem(st.nextToken()));
                    }
                    writer.newLine();
                }
            }
        } finally {
            if (modelIn != null) { try { modelIn.close(); } catch (Exception e) {} }
            if (reader != null) { try { reader.close(); } catch (Exception e) {} }
            if (writer != null) { try { writer.close(); } catch (Exception e) {} }
        }
    }
}
