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

import org.komodo.relational.RelationalObject.Filter;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.ArgCheck;
import org.modeshape.jcr.JcrLexicon;

/**
 * A filter for excluding descriptors and properties from specific namespaces.
 */
public class ExcludeNamespaceFilter implements Filter {

    private final String[] prefixes;

    /**
     * @param namespacePrefix
     *        the namespace prefix (cannot be empty)
     * @param namespaceUri
     *        the namespace URI (cannot be empty)
     */
    public ExcludeNamespaceFilter( final String namespacePrefix,
                                   final String namespaceUri ) {
        ArgCheck.isNotNull( namespacePrefix, "namespacePrefix" ); //$NON-NLS-1$
        ArgCheck.isNotNull( namespaceUri, "namespaceUri" ); //$NON-NLS-1$

        this.prefixes = new String[ 2 ];
        this.prefixes[0] = ( namespacePrefix + StringConstants.COLON );
        this.prefixes[1] = ( StringConstants.OPEN_BRACE + JcrLexicon.Namespace.URI + StringConstants.CLOSE_BRACE );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject.Filter#rejectDescriptor(java.lang.String)
     */
    @Override
    public boolean rejectDescriptor( final String descriptorName ) {
        ArgCheck.isNotEmpty( descriptorName, "descriptorName" ); //$NON-NLS-1$
        return ( descriptorName.startsWith( this.prefixes[0] ) || descriptorName.startsWith( this.prefixes[1] ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject.Filter#rejectProperty(java.lang.String)
     */
    @Override
    public boolean rejectProperty( final String propName ) {
        ArgCheck.isNotEmpty( propName, "propName" ); //$NON-NLS-1$
        return ( propName.startsWith( this.prefixes[0] ) || propName.startsWith( this.prefixes[1] ) );
    }

}
