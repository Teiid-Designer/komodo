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
package org.komodo.utils.i18n;

import java.io.InputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Formatter;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;

/**
 * A class that can be subclassed and used to internationalize property files. All public, static, and non-final strings in the
 * subclass will be set to the default locale's value in the associated property file value. The placeholders used in the messages
 * are those that are used by {@link Formatter}.
 */
public abstract class I18n {

    private class I18nProperties extends Properties {

        private static final long serialVersionUID = 297271848138379264L;

        private final Class< ? extends I18n > clazz;
        private final Collection< String > errors;
        private final Map< String, Field > fields;

        I18nProperties( final Map< String, Field > fields,
                        final Class< ? extends I18n > clazz,
                        final Collection< String > errors ) {
            this.clazz = clazz;
            this.fields = fields;
            this.errors = errors;
        }

        /**
         * {@inheritDoc}
         *
         * @see java.util.Hashtable#put(java.lang.Object, java.lang.Object)
         */
        @Override
        public synchronized Object put( final Object key,
                                        final Object value ) {
            if ( this.fields.containsKey( key ) ) {
                try {
                    final Field field = this.clazz.getDeclaredField( ( String )key );
                    field.set( null, value );
                    this.fields.remove( key );
                } catch ( final Exception e ) {
                    this.errors.add( I18n.bind( UtilsI18n.problemAccessingI18Field, key, this.clazz.getName() ) );
                }
            } else {
                this.errors.add( I18n.bind( UtilsI18n.missingI18Field, key, this.clazz.getName() ) );
            }

            return super.put( key, value );
        }

    }

    private static final KLog LOGGER = KLog.getLogger();

    /**
     * @param pattern
     *        the message pattern (cannot be <code>null</code> or empty)
     * @param args
     *        the arguments being used to replace placeholders in the message (can be <code>null</code> or empty)
     * @return the localized message (never <code>null</code>)
     */
    public static String bind( final String pattern,
                               final Object... args ) {
        ArgCheck.isNotEmpty( pattern, "pattern" ); //$NON-NLS-1$
        return String.format( pattern, args );
    }

    /**
     * Should be called in a <code>static</code> block to load the properties file and assign values to the class string fields.
     *
     * @throws IllegalStateException
     *         if there is a problem reading the I8n class file or properties file
     */
    protected void initialize() {
        final Map< String, Field > fields = new HashMap< String, Field >();

        // collect all public, static, non-final, string fields
        try {
            for ( final Field field : getClass().getDeclaredFields() ) {
                final int modifiers = field.getModifiers();

                if ( ( field.getType() == String.class )
                     && ( ( modifiers & Modifier.PUBLIC ) == Modifier.PUBLIC )
                     && ( ( modifiers & Modifier.STATIC ) == Modifier.STATIC )
                     && ( ( modifiers & Modifier.FINAL ) != Modifier.FINAL ) ) {
                    fields.put( field.getName(), field );
                }
            }
        } catch ( final Exception e ) {
            throw new IllegalStateException( I18n.bind( UtilsI18n.problemLoadingI18nClass, getClass().getName() ), e );
        }

        // load properties file
        IllegalStateException problem = null;
        final Class< ? extends I18n > thisClass = getClass();
        final String bundleName = thisClass.getName().replaceAll( "\\.", "/" ).concat( ".properties" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        final Collection< String > errors = new ArrayList< String >();
        final Properties props = new I18nProperties( fields, thisClass, errors );

        try ( final InputStream stream = thisClass.getClassLoader().getResource( bundleName ).openStream() ) {
            props.load( stream );

            // log errors for any properties keys that don't have fields
            for ( final String error : errors ) {
                if ( problem == null ) {
                    problem = new IllegalStateException( error );
                }

                LOGGER.error( error );
            }

            // log errors for any fields that don't have properties
            for ( final String fieldName : fields.keySet() ) {
                final String error = I18n.bind( UtilsI18n.missingPropertiesKey, fieldName, getClass().getName() );

                if ( problem == null ) {
                    problem = new IllegalStateException( error );
                }

                LOGGER.error( error );
            }
        } catch ( final Exception e ) {
            throw new IllegalStateException( I18n.bind( UtilsI18n.problemLoadingI18nProperties, getClass().getName() ), e );
        } finally {
            if ( problem != null ) {
                throw problem;
            }
        }
    }

}
