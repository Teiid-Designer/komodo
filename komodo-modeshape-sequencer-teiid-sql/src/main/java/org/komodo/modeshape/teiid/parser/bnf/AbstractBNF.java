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
package org.komodo.modeshape.teiid.parser.bnf;

import java.util.ArrayList;
import java.util.List;
import org.komodo.modeshape.teiid.parser.completion.TeiidCompletionParserConstants;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;

/**
 * Abstract implementation that {@link BNF} extends.
 */
public abstract class AbstractBNF implements BNFConstants, TeiidCompletionParserConstants {

    private final TeiidVersion version;

    /**
     * Create a new instance
     *
     * @param version teiid version
     */
    public AbstractBNF(TeiidVersion version) {
        this.version = version;
    }

    /**
     * @return the version
     */
    public TeiidVersion getVersion() {
        return this.version;
    }

    protected List<String> newList() {
        return new ArrayList<String>();
    }

    protected final int concat(int... indices) {
        if (indices == null)
            return -1;

        if (indices.length == 0)
            return 0;

        if (indices.length == 1)
            return indices[0];

        StringBuilder builder = new StringBuilder();
        for (int index : indices) {
            builder.append(index);
        }

        return Integer.valueOf(builder.toString());
    }

    protected void append(List<String> bnf, String value) {
        if (value == null)
            return;

        bnf.add(value);
    }

    protected void append(List<String> bnf, String[] values) {
        if (values == null)
            return;

        for (String value : values)
            append(bnf, value);
    }

    protected String[] array(List<String> bnf) {
        return bnf.toArray(new String[0]);
    }

    /**
     * The version of this parser must be greater than
     * or equal to the given version.
     *
     * @param requiredVersionEnum
     */
    protected boolean versionLessThan(Version requiredVersionEnum) {
        return getVersion().isLessThan(requiredVersionEnum.get());
    }

    /**
     * The version of this parser must be  than
     * or equal to the given version.
     *
     * @param requiredVersionEnum
     */
    protected boolean versionAtLeast(Version requiredVersionEnum) {
        return ! versionLessThan(requiredVersionEnum);
    }
}
