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
package org.komodo.relational;

import java.util.ArrayList;
import java.util.List;
import javax.xml.namespace.QName;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.utils.ArgCheck;

/**
 * A filter for excluding specific {@link QName}s.
 */
public class ExcludeQNamesFilter implements Filter {

    private final List< String > qnames;

    /**
     * Constructs a filter.
     *
     * @param namesToExclude
     *        a collection of names being excluded (cannot be <code>null</code> or empty)
     */
    public ExcludeQNamesFilter( final String... namesToExclude ) {
        ArgCheck.isNotEmpty( namesToExclude, "qnames" ); //$NON-NLS-1$
        this.qnames = new ArrayList<>( namesToExclude.length );

        for ( final String name : namesToExclude ) {
            this.qnames.add( name );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject.Filter#rejectDescriptor(java.lang.String)
     */
    @Override
    public boolean rejectDescriptor( final String descriptorName ) {
        ArgCheck.isNotEmpty( descriptorName, "descriptorName" ); //$NON-NLS-1$
        return this.qnames.contains( descriptorName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject.Filter#rejectProperty(java.lang.String)
     */
    @Override
    public boolean rejectProperty( final String propName ) {
        ArgCheck.isNotEmpty( propName, "propName" ); //$NON-NLS-1$
        return this.qnames.contains( propName );
    }

}
