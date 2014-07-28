/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.core;

import org.komodo.spi.outcome.IOutcome;
import org.komodo.utils.ArgCheck;


/**
 * A representation of Teiid relational data types.
 */
public class DataType implements DataTypes {

    @SuppressWarnings("javadoc")
    public static final String DEFAULT_NAME = "STRING"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_LENGTH = 0;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_PRECISION = 0;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_NUMERIC_PRECISION = 1;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_RADIX = 0;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_SCALE = 0;
    
    private String name = DEFAULT_NAME;
    private long length = DEFAULT_LENGTH;
    private int precision = DEFAULT_PRECISION;
    private int scale = DEFAULT_SCALE;
    private int radix = DEFAULT_RADIX;

    private DataTypeValidator validator = new DefaultDataTypeValidator();
    
    /**
     * DataType constructor
     */
    public DataType() {
        super();
    }

    /**
     * Constructor with name
     * @param name DataType name
     */
    public DataType( String name ) {
        super();
        setName(name);
    }

    /**
     * Constructor with name and length
     * @param name the DataType name
     * @param length the DataType length
     */
    public DataType( String name,
                     int length ) {
        super();
        setName(name);
        this.length = length;
    }

    /**
     * Constructor with name, precision and scale
     * @param name the DataType name
     * @param precision the DataType precision
     * @param scale the DataType scale
     */
    public DataType( String name,
                     int precision,
                     int scale ) {
        super();
        setName(name);
        this.precision = precision;
        this.scale = scale;
    }

    /**
     * Set the DataType name
     * @param value the DataType name
     */
    public void setName( String value ) {
        this.name = value;
        if( this.precision == DEFAULT_PRECISION &&
        	(this.name.equalsIgnoreCase(INTEGER) || 
        	this.name.equalsIgnoreCase(DECIMAL) || 
        	this.name.equalsIgnoreCase(LONG) || 
        	this.name.equalsIgnoreCase(SHORT) || 
        	this.name.equalsIgnoreCase(BIGDECIMAL) || 
        	this.name.equalsIgnoreCase(BIGINTEGER)) ) { 
        	setPrecision(DEFAULT_NUMERIC_PRECISION);
        }
    }

    /**
     * Get the DataType name
     * @return the DataType name
     */
    public String getName() {
        return this.name;
    }

    /**
     * Set the DataType length
     * @param value the DataType length
     */
    public void setLength( long value ) {
        this.length = value;
    }

    /**
     * Get the DataType length
     * @return the DataType length
     */
    public long getLength() {
        return this.length;
    }

    /**
     * Set the DataType precision
     * @param value the DataType precision
     */
    public void setPrecision( int value ) {
        this.precision = value;
    }

    /**
     * Get the DataType precision
     * @return the DataType precision
     */
    public int getPrecision() {
        return this.precision;
    }

    /**
     * Set the DataType scale
     * @param value the DataType scale
     */
    public void setScale( int value ) {
        this.scale = value;
    }

    /**
     * Get the DataType scale
     * @return the DataType scale
     */
    public int getScale() {
        return this.scale;
    }

    /**
     * Set the DataType radix
     * @param value the DataType radix
     */
    public void setRadix( int value ) {
        this.radix = value;
    }

    /**
     * Get the DataType radix
     * @return the DataType radix
     */
    public int getRadix() {
        return this.radix;
    }
    
    /**
     * @param validator the datatype validator
     * 
     */
    public void setValidator(DataTypeValidator validator) {
    	ArgCheck.isNotNull(validator, "validator"); //$NON-NLS-1$
    	this.validator = validator;
    }
    
    /**
     * @return the validation status
     */
    public boolean isValid() {
    	return validate().getLevel()!=IOutcome.Level.ERROR;
    }
    
    /**
     * @return the validation status
     */
    public IOutcome validate() {
    	return this.validator.validate(this);
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder(100);
        result.append("DataType()").append(" ").append(name) //$NON-NLS-1$ //$NON-NLS-2$
        .append(", Length: "+getLength()) //$NON-NLS-1$
        .append(", Precision: "+getPrecision()) //$NON-NLS-1$
        .append(", Scale: "+getScale()) //$NON-NLS-1$
        .append(", Radix: "+getRadix()); //$NON-NLS-1$

        return result.toString();
    }

}
