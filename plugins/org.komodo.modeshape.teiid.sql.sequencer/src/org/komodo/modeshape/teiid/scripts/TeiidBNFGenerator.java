/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.modeshape.teiid.scripts;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.komodo.modeshape.teiid.parser.bnf.AbstractBNF;
import org.komodo.modeshape.teiid.parser.bnf.BNFConstants;
import org.komodo.modeshape.teiid.parser.bnf.clause.BracketClause;
import org.komodo.modeshape.teiid.parser.bnf.clause.CaseStatement;
import org.komodo.modeshape.teiid.parser.bnf.clause.ClauseStack;
import org.komodo.modeshape.teiid.parser.bnf.clause.IClause;
import org.komodo.modeshape.teiid.parser.bnf.clause.IGroupClause;
import org.komodo.modeshape.teiid.parser.bnf.clause.OptionalClause;
import org.komodo.modeshape.teiid.parser.bnf.clause.OrClause;
import org.komodo.modeshape.teiid.parser.bnf.clause.TokenClause;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.utils.ArgCheck;

/**
 *
 */
@SuppressWarnings({"nls", "javadoc"})
public class TeiidBNFGenerator implements StringConstants {

    private static final String EXEC_HOME = DOT;

    private static final String SRC_DIR = EXEC_HOME + File.separator + SRC;

    private static final String JAVACC_DIR = SRC_DIR + File.separator + "javacc";
    
    private static final String BNF_DIR = SRC_DIR + File.separator +
                                                                    convertPackageToDirPath(BNFConstants.class.getPackage());

    private static final String BNF_FILENAME = "BNF.java";

    private static final String TEIID_SQL_JJT = "TeiidSyntaxParser.jjt";

    private static final Pattern METHOD_DEC_PATTERN = Pattern.compile("[A-Za-z]+ ([a-z][a-zA-Z]+)\\([a-zA-Z ]*\\) :");

    private static final Pattern COMMENT_PATTERN = Pattern.compile("[\\s]*([\\/\\/]|[\\/\\*]|[\\*]|[\\*\\/])+.*");

    private static final Pattern TRY_PATTERN = Pattern.compile("[\\s]*try.*");

    private static final Pattern CATCH_PATTERN = Pattern.compile("[\\s]*catch.*");

    private static final Pattern BRACE_PATTERN = Pattern.compile("[\\s]*[\\{\\}][\\s]*");

    private static final String LICENSE = "" +
    "/*" + NEW_LINE +
    " * JBoss, Home of Professional Open Source." + NEW_LINE +
    " * See the COPYRIGHT.txt file distributed with this work for information" + NEW_LINE +
    " * regarding copyright ownership.  Some portions may be licensed" + NEW_LINE +
    " * to Red Hat, Inc. under one or more contributor license agreements." + NEW_LINE +
    " * " + NEW_LINE +
    " * This library is free software; you can redistribute it and/or" + NEW_LINE +
    " * modify it under the terms of the GNU Lesser General Public" + NEW_LINE +
    " * License as published by the Free Software Foundation; either" + NEW_LINE +
    " * version 2.1 of the License, or (at your option) any later version." + NEW_LINE +
    " * " + NEW_LINE +
    " * This library is distributed in the hope that it will be useful," + NEW_LINE +
    " * but WITHOUT ANY WARRANTY; without even the implied warranty of" + NEW_LINE +
    " * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU" + NEW_LINE +
    " * Lesser General Public License for more details." + NEW_LINE +
    " * " + NEW_LINE +
    " * You should have received a copy of the GNU Lesser General Public" + NEW_LINE +
    " * License along with this library; if not, write to the Free Software" + NEW_LINE +
    " * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA" + NEW_LINE +
    " * 02110-1301 USA." + NEW_LINE +
    " */" + NEW_LINE;

    private class TokenModel {

        private StringBuffer content = new StringBuffer();
        
        public void addContent(String line) {
            content.append(line);
        }

        public String getContent() {
            return content.toString();
        }
    }

    private class MethodModel {
        private final String name;

        private List<String> contents;

        private Map<String, String> tokenMap;

        private ClauseStack clauseStack;

        public MethodModel(String name) {
            this.name = name;
            contents = new ArrayList<String>();
        }

        /**
         * @param tokenMap
         */
        public void setTokenMap(Map<String, String> tokenMap) {
            this.tokenMap = tokenMap;
        }

        private void createClauses() throws Exception {
            // < COMMA >
            // stringVal()

            Pattern tokenPattern = Pattern.compile("<([A-Z_0-9]+)>");
            Pattern functionPattern = Pattern.compile("([a-zA-Z]+)\\(.*\\)");
            Pattern ppPattern = Pattern.compile("pp[AS]+[a-z]+\\(bnf\\." + name + "\\((.*?)\\).*");
            Pattern reqPattern = Pattern.compile("requiresVersionAtLeast\\((Version.TEIID_[0-9]_[0-9])\\);");

            StringTokenizer tokenizer = new StringTokenizer(getContent());

            String identifier = null;
            clauseStack = new ClauseStack(IClause.ROOT_CLAUSE);

            while(tokenizer.hasMoreTokens()) {
                String token = tokenizer.nextToken();

                if (token.startsWith("LOOKAHEAD")) {
                    // Need to check if there are brackets with it
                    // If not then we need to ignore the next
                    int brackets = 0, iterations = 0;
                    do {
                        boolean hasBrackets = token.contains(OPEN_BRACKET);
                        if (hasBrackets) {
                            brackets = checkBrackets(token);
                            if (brackets == 0)
                                break;
                        } else
                            brackets = 1; // artificial setting to keep the loop going since expect at least 1 open-bracket

                        if (iterations > 10)
                            throw new Exception(token + " has an uneven number of brackets. Tried to continue but something really has gone wrong!");

                        token = token + tokenizer.nextToken();
                        iterations++;
                    } while(brackets > 0);

                    continue; // Want to totally ignore the LOOKAHEAD functions

                } else if (token.startsWith("ppAppend") || token.startsWith("ppSet")) {
                    // The tokeniser can split up pp function statements due to the commas/spaces
                    int brackets = 0, iterations = 0;
                    do {
                        boolean hasBrackets = token.contains(OPEN_BRACKET);
                        if (hasBrackets) {
                            brackets = checkBrackets(token);
                            if (brackets == 0)
                                break;
                        } else
                            brackets = 1; // artificial setting to keep the loop going since expect at least 1 open-bracket

                        if (iterations > 10)
                            throw new Exception(token + " has an uneven number of brackets. Tried to continue but something really has gone wrong!");

                        token = token + tokenizer.nextToken();
                        iterations++;
                    } while(brackets > 0);
                }

                //
                // Now the token has been put back together, test it against the expected matchers
                //
                Matcher tokenMatcher = tokenPattern.matcher(token);
                Matcher functionMatcher = functionPattern.matcher(token);
                Matcher ppMatcher = ppPattern.matcher(token);
                Matcher reqMatcher = reqPattern.matcher(token);

                if (tokenMatcher.matches()) {
                    identifier = tokenMatcher.group(1);
                    ArgCheck.isNotNull(identifier);
                    identifier = identifier.trim();

                    // Ignore the <EOF> token
                    if ("EOF".equals(identifier))
                        continue;

                    String tokenValue = tokenMap.get(identifier);
                    TokenClause clause = new TokenClause(identifier, false);
                    clause.setValue(tokenValue);
                    clauseStack.push(clause);

                } else if (functionMatcher.matches()) {
                    identifier = functionMatcher.group(1);
                    ArgCheck.isNotNull(identifier);
                    identifier = identifier.trim();

                    TokenClause clause = new TokenClause(identifier, true);
                    clause.setValue(identifier + "(0)");
                    clauseStack.push(clause);

                } else if (ppMatcher.matches()) {
                    String ppFunction = ppMatcher.group(1);
                    TokenClause clause = clauseStack.expectedLastClause(TokenClause.class);
                    if (clause != null)
                        clause.setPPFunction(ppFunction);

                } else if (token.equals(OPEN_BRACKET)) {
                    BracketClause clause = new BracketClause();
                    clauseStack.push(clause);

                } else if (token.equals(CLOSE_BRACKET)) {
                    BracketClause clause = clauseStack.getLatestOpenGroupClause(BracketClause.class);
                    if (clause != null) {
                        BracketClause bClause = clause;
                        // The CLOSE_BRACKET may not refer to this clause
                        // but to an inner clause first
                        bClause.closeClause(BracketClause.class);
                    }

                } else if (token.equals(CLOSE_BRACKET + STAR)) {
                    BracketClause clause = clauseStack.getLatestOpenGroupClause(BracketClause.class);
                    if (clause != null) {
                        BracketClause bClause = clause;
                        bClause.setMulti(true);
                        bClause.closeClause(BracketClause.class);
                    }

                } else if (token.equals(OPEN_SQUARE_BRACKET)) {
                    OptionalClause clause = new OptionalClause();
                    clauseStack.push(clause);

                } else if (token.equals(CLOSE_SQUARE_BRACKET)) {
                    OptionalClause clause = clauseStack.getLatestOpenGroupClause(OptionalClause.class);
                    if (clause != null) {
                        OptionalClause oClause = clause;
                        // The CLOSE__SQUARE_BRACKET may not refer to this clause
                        // but to an inner clause first
                        oClause.closeClause(OptionalClause.class);
                    }

                } else if (token.equals(PIPE)) {
                    OrClause orClause = new OrClause();
                    IClause topClause = clauseStack.peek();
                    if (topClause instanceof IGroupClause && ((IGroupClause) topClause).isOpen()) {
                        // Or clause belongs inside the topClause
                        IGroupClause gClause = (IGroupClause) topClause;
                        gClause.addClause(orClause);
                    } else {
                        // topClause is closed so push the orClause onto the stack
                        clauseStack.push(orClause);
                    }

                } else if (reqMatcher.matches()) {
                    String version = reqMatcher.group(1);
                    ArgCheck.isNotNull(version);

                    // requiresVersionAtLeast(Version.TEIID_8_4);
                    TokenClause clause = clauseStack.expectedLastClause(TokenClause.class);
                    if (clause != null)
                        clause.setMinimumVersion(version);
                }
            }
        }

        private int checkBrackets(String token) {
            int brackets = 0;
            for (int i = 0; i < token.length(); ++i) {
                char c = token.charAt(i);
                if (c == '(')
                    brackets++;
                else if (c == ')')
                    brackets--;
            }
            return brackets;
        }

        public void addContent(String line) throws Exception {
            contents.add(line);
        }

        public String getName() {
            return this.name;
        }

        public List<String> getContents() {
            return Collections.unmodifiableList(contents);
        }

        public String getContent() {
            StringBuffer buf = new StringBuffer();

            for (String line : getContents()) {
                line = line.replaceAll("[\\s]+", SPACE);

                // Replace all < KEY > with <KEY>
                line = line.replaceAll("< ([A-Z_0-9]+) >", "<$1>");
                buf.append(line);
            }

            return buf.toString();
        }

        public ClauseStack getClauseStack() throws Exception {
            if (clauseStack == null) {
                createClauses();
            }

            return clauseStack;
        }

        public boolean requiresSwitch() throws Exception {
            if (requiresIfElse())
                return false;

            ClauseStack clauseStack = getClauseStack();
            for (IClause clause : clauseStack) {
                if (clause.hasPPFunction())
                    return true;
            }

            return false;
        }

        /**
         * Detects a ppSet or ppAppend with 2 or more parameters
         * ppAppend(bnf.alter(BNF.VIEW, BNF.id));
         * 
         * @return content has a pp method with 2+ parameters
         */
        public boolean requiresIfElse() throws Exception {
            ClauseStack clauseStack = getClauseStack();
            for (IClause clause : clauseStack) {
                if (clause.hasMultiParameterPPFunction())
                    return true;
            }

            return false;
        }

    }

    private final BufferedReader ccReader;

    private final BufferedWriter bnfWriter;

    private static String convertPackageToDirPath(Package pkg) {
        return pkg.getName().replaceAll(DOUBLE_BACK_SLASH + DOT, File.separator);
    }

    /**
     * 
     */
    public TeiidBNFGenerator() throws Exception {

        File templateFile = new File(JAVACC_DIR, TEIID_SQL_JJT);
        ArgCheck.isTrue(templateFile.exists(), "JJT template cannot be found!");

        File bnfFile = new File(BNF_DIR, BNF_FILENAME);

        ccReader = new BufferedReader(new FileReader(templateFile));
        bnfWriter = new BufferedWriter(new FileWriter(bnfFile));
    }

    /**
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
        TeiidBNFGenerator generator = new TeiidBNFGenerator();
        generator.generate();
    }

    private void append(String line) throws Exception {
        bnfWriter.write(line);
    }

    private boolean isComment(String line) {
        Matcher m = COMMENT_PATTERN.matcher(line);
        return m.matches();
    }

    private boolean isStartComment(String line) {
        Matcher m = Pattern.compile("[\\s]*\\/\\*.*").matcher(line);
        return m.matches();
    }

    private boolean isEndComment(String line) {
        Matcher m = Pattern.compile("[\\s]*\\*\\/.*").matcher(line);
        return m.matches();
    }

    private boolean isTryOrCatch(String line) {
        Matcher mTry = TRY_PATTERN.matcher(line);
        Matcher mCatch = CATCH_PATTERN.matcher(line);
        return mTry.matches() || mCatch.matches();
    }

    private boolean isSingleBrace(String line) {
        Matcher mBrace = BRACE_PATTERN.matcher(line);
        return mBrace.matches();        
    }

    private boolean isDoubleBrace(String line) {
        return line.contains("{}");
    }

    private boolean isReturnStatement(String line) {
        return line.contains("return ");
    }

    private boolean isTokenDeclaration(String line) {
        return line.endsWith(COLON) && line.startsWith("TOKEN :");
    }

    private boolean isMethodDeclaration(String line) {
        Pattern p = Pattern.compile("[A-Za-z]+ [A-Za-z]+\\(.*\\) :");
        Matcher m = p.matcher(line);
        return m.matches();
    }

    private boolean isPPSetNullMethod(String line) {
        return line.contains("ppSet(null);");
    }

    private String extractMethodName(String line) {
        // void stringVal() :
        // Command command(ParseInfo info) :
        // void unsignedNumericLiteral() :
        
        if (! line.endsWith(COLON))
            return null;

        Matcher m = METHOD_DEC_PATTERN.matcher(line);
        if (! m.matches())
            return null;

        return m.group(1);
    }

    private void writeLicence() throws Exception {
        append(LICENSE);
    }

    private void writePackageDeclaration() throws Exception {
        Package p = BNFConstants.class.getPackage();
        append("package " + p.getName() + SEMI_COLON + NEW_LINE + NEW_LINE);
    }

    private void writeImports() throws Exception {
        String imp = "import";

        Class<?>[] klazzes = { List.class,
                                             ITeiidVersion.class,
                                             TeiidVersion.Version.class
                                            };

        for (Class<?> klazz : klazzes) {
            append(imp + SPACE + klazz.getCanonicalName() + SEMI_COLON + NEW_LINE);
        }

        append(NEW_LINE);
    }

    private void writeClassDeclaration() throws Exception {
        StringBuffer buf = new StringBuffer();
        buf.append("/**" + NEW_LINE)
            .append(" *" + NEW_LINE)
            .append(" */" + NEW_LINE)
            .append("@SuppressWarnings({\"static-access\", \"nls\"})" + NEW_LINE)
            .append(PUBLIC + SPACE + CLASS + SPACE + "BNF" + SPACE + " extends ")
            .append(AbstractBNF.class.getSimpleName() + SPACE + OPEN_BRACE + NEW_LINE)
            .append(NEW_LINE)
            .append(TAB + "/**" + NEW_LINE)
            .append(TAB + " * @param version" + NEW_LINE)
            .append(TAB + " */" + NEW_LINE)
            .append(TAB + PUBLIC + SPACE + "BNF" + OPEN_BRACKET)
            .append(ITeiidVersion.class.getSimpleName() + SPACE + "version" + CLOSE_BRACKET)
            .append(SPACE + OPEN_BRACE + NEW_LINE)
            .append(TAB + TAB + "super(version)" + SEMI_COLON + NEW_LINE)
            .append(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE);

        append(buf.toString());
    }

    /*
     * Iterate over the found tokens in the map in order to substitute
     * any tokens used in other tokens for their values
     */
    private boolean removeSubstitutions(Map<String, String> tokenMap) {
        boolean subsMade = false;
        Pattern tokenPattern = Pattern.compile("<([A-Z_]+)>");

        Iterator<Entry<String, String>> iterator = tokenMap.entrySet().iterator();
        while(iterator.hasNext()) {
            Entry<String, String> entry = iterator.next();
            Matcher m = tokenPattern.matcher(entry.getValue());
            if (!m.find())
                continue;

            StringBuffer buf = new StringBuffer();

            // Does contain one or more tokens
            do {
                String replacement = tokenMap.get(m.group(1));
                m.appendReplacement(buf, replacement);
            } while (m.find());

            m.appendTail(buf);

            String varValue = buf.toString();
            // Properly escape speech marks by adding backslashed then remove any extra
            // backslashes erroneously added
            varValue = varValue.replaceAll("\"", "\\\\\"");
            varValue = varValue.replaceAll("\\\\\\\\\"", "\\\\\"");

            tokenMap.put(entry.getKey(), varValue);
            subsMade = true;
        }

        return subsMade;
    }

    private Map<String, String> analyseTokens(List<TokenModel> tokens) throws Exception {
        Map<String, String> tokenMap = new HashMap<String, String>();

        for (TokenModel tokenModel : tokens) {
            String contents = tokenModel.getContent();

            // Replace all tabs and other whitespace with SPACES
            contents = contents.replaceAll("[\\s]+", SPACE);

            // Replace all < KEY > with <KEY>
            contents = contents.replaceAll("< ([A-Z_]+) >", "<$1>");

            // Replace all bracket + space with just a bracket
            contents = contents.replaceAll("\\( ", "\\(");

            Stack<Character> bStack = new Stack<Character>();
            StringBuffer vNameBuf = new StringBuffer();
            StringBuffer vValueBuf = new StringBuffer();
            StringBuffer currBuf = null;
            boolean inSpeechMarks = false;
            for (int i = 0; i < contents.length(); ++i) {
                char c = contents.charAt(i);

                if (c == '"') {
                    Character ce1 = null;
                    if (i > 0)
                        ce1 = contents.charAt(i - 1);

                    // Ignore escaped speech marks
                    if (ce1 == null || ! ce1.equals('\\'))
                        inSpeechMarks = !inSpeechMarks;
                }
                else if (c == '<' && !inSpeechMarks) {
                    bStack.push(c);

                    if (currBuf == null) {
                        currBuf = vNameBuf;
                        // Ensures no leading < in varName
                        continue;
                    }

                } else if (c == '>' && !inSpeechMarks)
                    bStack.pop();

                if (bStack.isEmpty() && currBuf == vValueBuf) {
                    currBuf = null;
                    String varName = vNameBuf.toString().trim();
                    if (varName.startsWith(HASH))
                        varName = varName.substring(1);

                    String varValue = vValueBuf.toString().trim();
                    // Properly escape speech marks by adding backslashed then remove any extra
                    // backslashes erroneously added
                    varValue = varValue.replaceAll("\"", "\\\\\"");
                    varValue = varValue.replaceAll("\\\\\\\\\"", "\\\\\"");
                    tokenMap.put(varName, varValue);

                    vNameBuf = new StringBuffer();
                    vValueBuf = new StringBuffer();
                    continue;
                }

                if (c == ':' && !inSpeechMarks) {
                    currBuf = vValueBuf;
                    continue;
                }

                if (currBuf != null)
                currBuf.append(c);
            }
        }

        boolean removeSubs = true;
        while(removeSubs) {
            removeSubs = removeSubstitutions(tokenMap);
        }

        return tokenMap;
    }

    private Collection<TokenClause> nextTokenClauses(IClause clause) {
        LinkedHashSet<TokenClause> tokenClauses = new LinkedHashSet<TokenClause>();
        IClause nextClause = clause.nextClause();

        //
        // Need to check that the owning stack's parent clause is a
        // bracket 1-Many clause. In which case the first clause in
        // the stack should also be the next clause.
        //
        ClauseStack owningStack = clause.getOwningStack();
        IClause parent = owningStack.getParent();
        IClause targetClause = clause;
        while(parent != IClause.ROOT_CLAUSE && parent != null) {
            if (parent instanceof BracketClause && ((BracketClause) parent).isMulti()) {
                //
                // Need to further check that the clause is the last in the stack
                //
                if (owningStack.isConsideredLastClause(targetClause))
                    tokenClauses.addAll(parent.getFirstTokenClauses());
            }

            owningStack = parent.getOwningStack();
            if (owningStack == null) {
                targetClause = null;
                parent = null;
            } else {
                targetClause = parent;
                parent = owningStack.getParent();
            }
        }

        if (nextClause == null)
            return tokenClauses;

        tokenClauses.addAll(nextClause.getFirstTokenClauses());

        if ((nextClause instanceof OptionalClause) || (nextClause instanceof BracketClause && ((BracketClause) nextClause).isMulti())) {
            //
            // An optional clause implies it can be missed out entirely hence the next clause
            // after it should also be added to as a possibility.
            //
            tokenClauses.addAll(nextTokenClauses(nextClause));
        }

        return tokenClauses;
    }

    private CaseStatement createCaseStatement(String ppFunction, MethodModel method) throws Exception {
        if (ppFunction == null)
            return null;

        CaseStatement caseStmt = new CaseStatement();

        if (method.requiresSwitch()) {
            // switch case statement
            caseStmt.addDeclaration(TAB + TAB + TAB + "case " + ppFunction + COLON + NEW_LINE);
            return caseStmt;
        }

        // if else statement
        
        // } else if (index == ALTER) {
        // } else if (index == concat(VIEW, AS)) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(TAB + TAB + CLOSE_BRACE + " else if (index == ");

        if (ppFunction.contains(COMMA))
            buffer.append("concat" + OPEN_BRACKET + ppFunction + CLOSE_BRACKET + CLOSE_BRACKET);
        else
            buffer.append(ppFunction + CLOSE_BRACKET);

        buffer.append(SPACE + OPEN_BRACE + NEW_LINE);

        caseStmt.addDeclaration(buffer.toString());

        return caseStmt;
    }
    
    private void appendRemainingTokenClauses(Collection<CaseStatement> caseStmts, MethodModel method, List<TokenClause> currClauses, Set<TokenClause> tokenCache) throws Exception {
        if (currClauses == null || currClauses.isEmpty())
            return;

        List<TokenClause> nextClauses = new ArrayList<TokenClause>();

        for (TokenClause currClause : currClauses) {
            if (tokenCache.contains(currClause))
                continue;

            tokenCache.add(currClause);

            // case currClause.ppFunction
            String ppFunction = currClause.getPPFunction();
            CaseStatement caseStmt = createCaseStatement(ppFunction, method);

            Collection<TokenClause> nextTokenClauses = nextTokenClauses(currClause);
            for (TokenClause nextTokenClause : nextTokenClauses) {

                if (caseStmt != null) {
                    StringBuffer appendStmt = new StringBuffer();
                    // append(bnf, nextClause.value)
                    if (method.requiresSwitch())
                        appendStmt.append(TAB + TAB + TAB + TAB);
                    else
                        appendStmt.append(TAB + TAB + TAB);

                    if (nextTokenClause.getMinVersion() != null)
                        appendStmt.append(nextTokenClause.getVersionStatement() + SPACE);

                    appendStmt.append(nextTokenClause.getAppendStatement() + NEW_LINE);
                    caseStmt.addStatement(appendStmt.toString());
                }

                nextClauses.add(nextTokenClause);
            }

            if (caseStmt != null)
                caseStmts.add(caseStmt);
        }

        appendRemainingTokenClauses(caseStmts, method, nextClauses, tokenCache);
    }

    private void analyseMethod(MethodModel method) throws Exception {
        StringBuffer buf = new StringBuffer();

        // Append method declaration
        buf.append(TAB + PUBLIC + SPACE + String.class.getSimpleName())
            .append(OPEN_SQUARE_BRACKET + CLOSE_SQUARE_BRACKET + SPACE)
            .append(method.getName() + OPEN_BRACKET + "int... indices" + CLOSE_BRACKET + SPACE + OPEN_BRACE)
            .append(NEW_LINE);

        // List declaration
        buf.append(TAB + TAB + "List<String> bnf = newList();" + NEW_LINE + NEW_LINE);

        ClauseStack clauseStack = method.getClauseStack();
        if (!method.requiresSwitch() && !method.requiresIfElse()) {
            for (IClause clause : clauseStack) {
                for (String appendStatement : clause.getAppendStatements()) {
                    buf.append(TAB + TAB + appendStatement + NEW_LINE);
                }
            }
        } else {
            //
            // int index = concat(indices);
            //
            buf.append(TAB + TAB + "int index = concat(indices);" + NEW_LINE);

            if (method.requiresIfElse()) {
                // if (index == 0) {
                buf.append(TAB + TAB + "if (index == 0) {" + NEW_LINE);
            } else {
                // switch (index) {
                //    case 0:            
                buf.append(TAB + TAB + "switch (index) {" + NEW_LINE);
                buf.append(TAB + TAB + TAB + "case 0:" + NEW_LINE);
            }

            IClause clause = clauseStack.get(0);
            List<TokenClause> firstClauses = clause.getFirstTokenClauses();
            for (TokenClause tokenClause : firstClauses) {

                if (method.requiresSwitch())
                    buf.append(TAB + TAB + TAB + TAB);
                else
                    buf.append(TAB + TAB + TAB);

                if (tokenClause.getMinVersion() != null)
                    buf.append(tokenClause.getVersionStatement() + SPACE);

                buf.append(tokenClause.getAppendStatement() + NEW_LINE);
            }

            if (method.requiresSwitch()) {
                // switch requires a break statement
                buf.append(IClause.BREAK_STATEMENT);
            }

            //
            // Using a hash set of case statement, we can deduplicate. This can happen if there
            // are multi-clauses in the method as well as a singular bracket clause
            //
            LinkedHashSet<CaseStatement> caseStatements = new LinkedHashSet<CaseStatement>();
            appendRemainingTokenClauses(caseStatements, method, firstClauses, new HashSet<TokenClause>());

            for (CaseStatement caseStmt : caseStatements) {
                for (String declaration : caseStmt.getDeclarations()) {
                    buf.append(declaration);
                }

                for (String statement : caseStmt.getStatements()) {
                    buf.append(statement);
                }

                if (method.requiresSwitch()) {
                    // break;
                    buf.append(IClause.BREAK_STATEMENT);
                }
            }
            
            buf.append(TAB + TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE);
        }


        // Return statement
        buf.append(TAB + TAB + "return array(bnf);" + NEW_LINE);
        
        // Append method closing brace
        buf.append(TAB + CLOSE_BRACE + NEW_LINE + NEW_LINE);

        append(buf.toString());
    }

    public void generate() throws Exception {
        try {

            String line = null;
            boolean startAnalysis = false;
            boolean inComment = false;
            List<MethodModel> methods = new ArrayList<MethodModel>();
            List<TokenModel> tokens = new ArrayList<TokenModel>();
            MethodModel currentMethod = null;
            TokenModel currentToken = null;

            while ((line = ccReader.readLine()) != null) {

                // Ignore everything about the parser's actual generated class
                if (line.startsWith("PARSER_END"))
                    startAnalysis = true;
                
                if (! startAnalysis)
                    continue;

                if (isStartComment(line))
                    inComment = true;

                if (isEndComment(line))
                    inComment = false;

                if (inComment)
                    continue;

                if (isTokenDeclaration(line)) {
                    currentToken = new TokenModel();
                    tokens.add(currentToken);

                    currentMethod = null;
                    continue;
                } else if (isMethodDeclaration(line)) {
                    String methodName = extractMethodName(line);
                    if (methodName == null)
                        currentMethod = null; // Not a proper method eg. TOKEN pragma
                    else {
                        currentMethod = new MethodModel(methodName);
                        methods.add(currentMethod);
                    }

                    currentToken = null;                    
                    continue;
                }

                if (currentMethod == null && currentToken == null)
                    continue;

                if (line.isEmpty())
                    continue;

                if (isComment(line))
                    // Ignore comments
                    continue;

                if (isTryOrCatch(line))
                    continue;

                if (isSingleBrace(line))
                    continue;

                if (isDoubleBrace(line))
                    continue;

                if (isReturnStatement(line))
                    continue;

                if (isPPSetNullMethod(line))
                    continue;

                if (currentMethod != null) {
                    currentMethod.addContent(line);
                    
                } else if (currentToken != null)
                    currentToken.addContent(line);
            }

            writeLicence();
            writePackageDeclaration();
            writeImports();
            writeClassDeclaration();

            Map<String, String> tokenMap = analyseTokens(tokens);

            for (MethodModel method : methods) {
                method.setTokenMap(tokenMap);
                analyseMethod(method);
            }

            append(NEW_LINE + CLOSE_BRACE);

        } finally {
            bnfWriter.close();
            ccReader.close();
        }
    }

}
