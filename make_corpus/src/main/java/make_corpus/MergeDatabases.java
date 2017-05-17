package make_corpus;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

/**
 * Created by serge on 5/17/2017.
 */
public class MergeDatabases {
    static {
        try {
            Class.forName("org.sqlite.JDBC");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /*         try {
                    threadMain("../src_data/en_US/en_US.blogs.txt", "../src_data/en_US/en_US.blogs.stats.db");
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }).start();
        new Thread(new Runnable() {
            public void run() {
                try {
                    threadMain("../src_data/en_US/en_US.news.txt", "../src_data/en_US/en_US.news.stats.db");
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }).start();
        new Thread(new Runnable() {
            public void run() {
                try {
                    threadMain("../src_data/en_US/en_US.twitter.txt", "../src_data/en_US/en_US.twitter.stats.db");*/
    public static void main(String[] argv) throws SQLException {
        File tmpFile = null;
        Connection tmpConn = null;
        Connection destConn = null;
        try {
            tmpFile = new File("../src_data/en_US/tmp" + System.currentTimeMillis() + ".db");
            tmpConn = initializeTmpDatabase(tmpFile);
            gatherAllRecords("../src_data/en_US/en_US.blogs.stats.db", tmpConn);
            gatherAllRecords("../src_data/en_US/en_US.news.stats.db", tmpConn);
            gatherAllRecords("../src_data/en_US/en_US.twitter.stats.db", tmpConn);
        } finally {
            if (tmpConn != null) {
                try {
                    tmpConn.close();
                } catch (Exception e) {
                    // swallow if any
                }
            }
            if (destConn != null) {
                try {
                    destConn.close();
                } catch (Exception e) {
                    // swallow if any
                }
            }
            if (tmpFile != null && tmpFile.exists()) {
                try {
                    tmpFile.delete();
                } catch (Exception e) {
                    // swallow if any
                }
            }
        }
    }

    private static void gatherAllRecords(String fileName, Connection tmpConn) throws SQLException {
        Connection srcConn = null;
        try {
            srcConn = openDatabase(fileName);
            
        } finally {
            try {
                srcConn.close();
            } catch (Exception e) {
                // swallow if any
            }
        }
    }

    private static Connection openDatabase(String fileName) throws SQLException {
        File outputFile = new File(fileName);
        String url = "jdbc:sqlite://" + outputFile.getAbsoluteFile();
        Connection conn = DriverManager.getConnection(url);
        return conn;
    }

    private static Connection initializeTmpDatabase(File outputFile) throws SQLException {
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

            conn.commit();
        } finally {
            if (stmt != null) try { stmt.close(); } catch (Exception e) {}
        }
        return conn;
    }
}
