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
package org.komodo.repository.search;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import org.komodo.spi.repository.Repository.KeywordCriteria;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * A Contains clause
 *
 * CONTAINS(tagged.[acme:tagName], 'foo')
 *
 */
class ContainsClause extends Clause implements AliasPropertyClause {

    private final Set<String> keywords = new LinkedHashSet<String>();

    private final KeywordCriteria keywordCriteria;

    /**
     * Constructor
     * @param parent parent searcher
     * @param operator the logical operator preceding this clause (can be null if this is the only clause)
     * @param alias the alias
     * @param property the property
     * @param keywordCriteria settings for the keywords
     * @param keywords the value
     */
    public ContainsClause(ObjectSearcher parent, LogicalOperator operator,
                                         String alias, String property,
                                         KeywordCriteria keywordCriteria, String... keywords) {
        super(parent, operator);

        ArgCheck.isNotNull(property);
        ArgCheck.isNotEmpty(keywords, "Where Contains clause requires at least 1 value"); //$NON-NLS-1$

        setAlias(alias);
        setProperty(PROPERTY, property);

        this.keywordCriteria = keywordCriteria == null ? KeywordCriteria.getDefault() : keywordCriteria;

        for (String keyword : keywords)
            this.keywords.add(keyword);
    }

    /**
     * @return the alias
     */
    @Override
    public String getAlias() {
        return properties.get(ALIAS);
    }

    /**
     * @param alias the alias to set
     */
    public void setAlias(String alias) {
        properties.put(ALIAS, alias);
    }

    /**
     * @return the property
     */
    @Override
    public String getProperty() {
        return properties.get(PROPERTY);
    }

    /**
     * @param keyword the keyword to be added
     */
    public void addKeyword(String keyword) {
        this.keywords.add(keyword);
    }

    /**
     * @return the keywords
     */
    public Set<String> getKeywords() {
        return this.keywords;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.keywordCriteria == null) ? 0 : this.keywordCriteria.hashCode());
        result = prime * result + ((this.keywords == null) ? 0 : this.keywords.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        ContainsClause other = (ContainsClause)obj;
        if (this.keywordCriteria != other.keywordCriteria)
            return false;
        if (this.keywords == null) {
            if (other.keywords != null)
                return false;
        } else if (!this.keywords.equals(other.keywords))
            return false;
        return true;
    }

    @Override
    public String clauseString(int index) {
        StringBuffer buffer = new StringBuffer();

        appendLogicalOperator(index, buffer);

        setAlias(checkWhereAlias(getAlias()));

        buffer.append("CONTAINS"); //$NON-NLS-1$
        buffer.append(OPEN_BRACKET);

        if (! StringUtils.isEmpty(getAlias())) {
            buffer.append(getAlias());
            buffer.append(DOT);
        }

        buffer.append(OPEN_SQUARE_BRACKET);
        buffer.append(getProperty());
        buffer.append(CLOSE_SQUARE_BRACKET);
        buffer.append(COMMA);
        buffer.append(SPACE);

        buffer.append(QUOTE_MARK);

        Iterator<String> iter  = getKeywords().iterator();
        while(iter.hasNext()) {
            String keyword = iter.next();

            if (KeywordCriteria.NONE == keywordCriteria)
                buffer.append(HYPHEN);

            buffer.append(keyword);

            if (iter.hasNext()) {
                buffer.append(SPACE);

                if (KeywordCriteria.ANY == keywordCriteria) {
                    buffer.append("OR"); //$NON-NLS-1$
                    buffer.append(SPACE);
                }
            }
        }

        buffer.append(QUOTE_MARK);

        buffer.append(CLOSE_BRACKET);

        return buffer.toString();
    }
}