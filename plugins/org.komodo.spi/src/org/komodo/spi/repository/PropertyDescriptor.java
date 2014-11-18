/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.spi.repository;

import org.komodo.spi.KException;

/**
 * A {@link KomodoObject Komodo object's} {@link Property property} descriptor.
 */
public interface PropertyDescriptor {

    /**
     * The property type.
     */
    public enum Type {

        /**
         * A binary property.
         */
        BINARY,

        /**
         * A boolean property.
         */
        BOOLEAN,

        /**
         * A date property.
         */
        DATE,

        /**
         * A decimal property.
         */
        DECIMAL,

        /**
         * A double property.
         */
        DOUBLE,

        /**
         * A long property.
         */
        LONG,

        /**
         * A name property has a namespace and a local name.
         */
        NAME,

        /**
         * A path property.
         */
        PATH,

        /**
         * A reference property that is used to enforce referential integrity
         */
        REFERENCE,

        /**
         * A string property.
         */
        STRING,

        /**
         * Property can be of any type.
         */
        UNDEFINED,

        /**
         * A string property whose value must conform to the URI syntax.
         */
        URI,

        /**
         * A reference property that is not used to enforce referential integrity.
         */
        WEAKREFERENCE

    }

    /**
     * An empty array of property descriptors.
     */
    PropertyDescriptor[] NO_DESCRIPTORS = {};

    /**
     * An empty array of objects.
     */
    Object[] NO_VALUES = {};

    /**
     * If the property is multi-valued, there can be more than one default value. For a single-valued property there can be at
     * most one default value.
     *
     * @return the default values or an empty array if no default values exist.
     * @throws KException
     *         if an error occurs
     */
    Object[] getDefaultValues() throws KException;

    /**
     * @return the property name (never <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    String getName() throws KException;

    /**
     * @return the type of the property (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Type getType() throws KException;

    /**
     * @return <code>true</code> if property is required
     * @throws KException
     *         if an error occurs
     */
    boolean isMandatory() throws KException;

    /**
     * @return <code>true</code> if the property value is modifiable
     * @throws KException
     *         if an error occurs
     */
    boolean isModifiable() throws KException;

    /**
     * @return <code>true</code> if this property is multi-valued
     * @throws KException
     *         if an error occurs
     */
    boolean isMultiple() throws KException;

}
