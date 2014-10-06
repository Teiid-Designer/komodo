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
package org.komodo.modeshape.teiid.parser.bnf.clause;

import java.util.Collections;
import java.util.List;

/**
 *
 */
public class TokenClause implements IClause {

    private final String identifier;

    private final boolean isFunction;

    private String value;

    private String ppFunction;

    private ClauseStack owningStack;

    private String minVersion;

    /**
     * @param identifier
     */
    public TokenClause(String identifier, boolean isFunction) {
        this.identifier = identifier;
        this.isFunction = isFunction;
    }

    @Override
    public ClauseStack getOwningStack() {
        return owningStack;
    }

    @Override
    public void setOwningStack(ClauseStack clauseStack) {
        this.owningStack = clauseStack;
    }

    @Override
    public IClause nextClause() {
        return getOwningStack().nextClause(this);
    }

    /**
     * @return identifier
     */
    public String getIdentifier() {
        return this.identifier;
    }

    /**
     * @return the value
     */
    public String getValue() {
        return this.value;
    }

    /**
     * @param value
     */
    public void setValue(String value) {
        this.value = value;
    }

    /**
     * @return the ppFunction
     */
    public String getPPFunction() {
        return this.ppFunction;
    }

    /**
     * @param ppFunction
     */
    public void setPPFunction(String ppFunction) {
        this.ppFunction = ppFunction;
    }

    @Override
    public <T extends IClause> T getLastClause(Class<T> clauseClass) {
        if (TokenClause.class.equals(clauseClass))
            return (T)this;

        return null;
    }

    @Override
    public <T extends IGroupClause> T findLatestOpenGroupClause(Class<T> groupClass) {
        return null;
    }

    @Override
    public List<TokenClause> getFirstTokenClauses() {
        return Collections.singletonList(this);
    }

    public String getVersionStatement() {
        if (minVersion == null)
            return EMPTY_STRING;

        return "if (versionAtLeast(" + minVersion +"))"; //$NON-NLS-1$ //$NON-NLS-2$
    }

    public String getAppendStatement() {
        //
        // append(bnf, NonReserved.INSTEAD);
        // append(bnf, BNF.stringVal);
        //
        StringBuffer buf = new StringBuffer();

        buf.append(BNF_APPEND_PREFIX);
        if (!isFunction)
            buf.append(SPEECH_MARK);

        String value = this.value.replaceAll("\\\\\\\"", EMPTY_STRING); //$NON-NLS-1$
        if (!isFunction)
            value = value.toUpperCase();

        buf.append(value);

        if (!isFunction)
            buf.append(SPEECH_MARK);

        buf.append(CLOSE_BRACKET + SEMI_COLON);
        return buf.toString();
    }

    @Override
    public List<String> getAppendStatements() {
        StringBuffer buffer = new StringBuffer();

        if (minVersion != null)
            buffer.append(getVersionStatement() + SPACE);

        buffer.append(getAppendStatement());

        return Collections.singletonList(buffer.toString());
    }

    @Override
    public boolean hasPPFunction() {
        return getPPFunction() != null;
    }

    @Override
    public boolean hasMultiParameterPPFunction() {
        if (!hasPPFunction())
            return false;

        return getPPFunction().contains(COMMA);
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append(getIdentifier() + SPACE);
        buf.append(OPEN_BRACE + SPACE + getValue() + SPACE + CLOSE_BRACE + SPACE);
        if (getPPFunction() != null)
            buf.append("ppSet|Append " + getPPFunction() + SPACE); //$NON-NLS-1$

        buf.append(TAB + TAB + owningStack.hashCode());
        return buf.toString();
    }

    /**
     * @return the minVersion
     */
    public String getMinVersion() {
        return this.minVersion;
    }

    /**
     * @param version
     */
    public void setMinimumVersion(String version) {
        this.minVersion = version;
    }
}
