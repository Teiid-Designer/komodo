/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import java.util.Properties;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.RELATIONAL;
import org.komodo.relational.core.RelationalStringNameValidator;
import org.komodo.spi.outcome.IOutcome;
import org.komodo.spi.outcome.IOutcome.Level;
import org.komodo.spi.outcome.OutcomeFactory;
import org.komodo.utils.HashCodeUtil;
import org.komodo.utils.StringUtil;



/**
 * 
 *
 * @since 8.0
 */
public class Parameter extends RelationalObject {
    @SuppressWarnings("javadoc")
    public static final String KEY_DATATYPE = "DATATYPE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_NATIVE_TYPE = "NATIVETYPE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_NULLABLE = "NULLABLE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_DIRECTION = "DIRECTION"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_DEFAULT_VALUE = "DEFAULTVALUE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_LENGTH = "LENGTH"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_PRECISION = "PRECISION"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_RADIX = "RADIX"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_SCALE = "SCALE"; //$NON-NLS-1$
    
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_DATATYPE = null;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_NATIVE_TYPE = null;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_NULLABLE = NULLABLE.NULLABLE;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_DIRECTION = DIRECTION.IN;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_DEFAULT_VALUE = null;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_LENGTH = 0;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_PRECISION = 0;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_RADIX = 0;
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_SCALE = 10;
    
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_STRING_LENGTH = 4000;
    
    private String  datatype;
    private String  nativeType;
    private String  nullable;
    private String  direction = DEFAULT_DIRECTION;
    private String  defaultValue;
    private int length;
    private int precision;
    private int radix;
    private int scale;
    
    /**
     * RelationalParameter constructor
     */
    public Parameter() {
        super();
        setNameValidator(new RelationalStringNameValidator(false));
    }
    
    /**
     * RelationalParameter constructor
     * @param name the parameter name
     */
    public Parameter( String name ) {
        super(name);
        setNameValidator(new RelationalStringNameValidator(false));
    }
    
    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.PARAMETER;
    }

    /**
     * @return datatype
     */
    public String getDatatype() {
        return datatype;
    }
    /**
     * @param datatype Sets datatype to the specified value.
     */
    public void setDatatype( String datatype ) {
        this.datatype = datatype;
    }
    /**
     * @return nativeType
     */
    public String getNativeType() {
        return nativeType;
    }
    /**
     * @param nativeType Sets nativeType to the specified value.
     */
    public void setNativeType( String nativeType ) {
        this.nativeType = nativeType;
    }
    /**
     * @return nullable
     */
    public String getNullable() {
        return nullable;
    }
    /**
     * @param nullable Sets nullable to the specified value.
     */
    public void setNullable( String nullable ) {
        this.nullable = nullable;
    }
    /**
     * @return direction
     */
    public String getDirection() {
        return direction;
    }
    /**
     * @param direction Sets direction to the specified value.
     */
    public void setDirection( String direction ) {
        this.direction = direction;
    }
    /**
     * @return defaultValue
     */
    public String getDefaultValue() {
        return defaultValue;
    }
    /**
     * @param defaultValue Sets defaultValue to the specified value.
     */
    public void setDefaultValue( String defaultValue ) {
        this.defaultValue = defaultValue;
    }
    /**
     * @return length
     */
    public int getLength() {
        return length;
    }
    /**
     * @param length Sets length to the specified value.
     */
    public void setLength( int length ) {
        this.length = length;
    }
    /**
     * @return precision
     */
    public int getPrecision() {
        return precision;
    }
    /**
     * @param precision Sets precision to the specified value.
     */
    public void setPrecision( int precision ) {
        this.precision = precision;
    }
    /**
     * @return radix
     */
    public int getRadix() {
        return radix;
    }
    /**
     * @param radix Sets radix to the specified value.
     */
    public void setRadix( int radix ) {
        this.radix = radix;
    }
    /**
     * @return scale
     */
    public int getScale() {
        return scale;
    }
    /**
     * @param scale Sets scale to the specified value.
     */
    public void setScale( int scale ) {
        this.scale = scale;
    }
    
	@Override
	public IOutcome validate() {
		// Walk through the properties for the parameter and set the status
		super.validate();
		
		if( getOutcome().getLevel() == Level.ERROR ) {
			return this.currentOutcome;
		}
		
		// Parameter directions check
		Procedure parentProcedure = (Procedure)getParent();
		if(parentProcedure!=null && parentProcedure.isFunction()) {
			if( ! getDirection().equalsIgnoreCase(DIRECTION.IN) &&
					! getDirection().equalsIgnoreCase(DIRECTION.RETURN)	) {
				this.currentOutcome = OutcomeFactory.getInstance().createError(
						Messages.getString(RELATIONAL.validate_error_invalidParameterDirectionInFunction) ); 
				return this.currentOutcome;
			}
		}
		return this.currentOutcome;
	}
    
    /**
     * Set properties
     * @param props the properties
     */
    @Override
	public void setProperties(Properties props) {
    	// Set common properties
    	super.setProperties(props);
    	
        for( Object key : props.keySet() ) {
            String keyStr = (String)key;
            String value = props.getProperty(keyStr);

            if( value != null && value.length() == 0 ) {
                continue;
            }
            
            if(keyStr.equalsIgnoreCase(KEY_LENGTH) ) {
                setLength(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_DATATYPE) ) {
                setDatatype(value);
            } else if(keyStr.equalsIgnoreCase(KEY_DEFAULT_VALUE) ) {
                setDefaultValue(value);
            } else if(keyStr.equalsIgnoreCase(KEY_DIRECTION) ) {
                setDirection(value);
            } else if(keyStr.equalsIgnoreCase(KEY_NATIVE_TYPE) ) {
                setNativeType(value);
            } else if(keyStr.equalsIgnoreCase(KEY_NULLABLE) ) {
                setNullable(value);
            } else if(keyStr.equalsIgnoreCase(KEY_PRECISION) ) {
                setPrecision(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_SCALE) ) {
                setScale(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_RADIX) ) {
                setRadix(Integer.parseInt(value));
            } 
        }
    	
        handleInfoChanged();
    }
    
    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object object ) {
		if (!super.equals(object)) {
			return false;
		}
        if (this == object)
            return true;
        if (object == null)
            return false;
        if (getClass() != object.getClass())
            return false;
        final Parameter other = (Parameter)object;

        // string properties
        if (!StringUtil.valuesAreEqual(getDatatype(), other.getDatatype()) ||
        		!StringUtil.valuesAreEqual(getDefaultValue(), other.getDefaultValue()) ||
        		!StringUtil.valuesAreEqual(getDirection(), other.getDirection()) ||
        		!StringUtil.valuesAreEqual(getNativeType(), other.getNativeType()) ||
        		!StringUtil.valuesAreEqual(getNullable(), other.getNullable()) ) {
        	return false;
        }
        
        if( !(getLength()==other.getLength()) ||
            !(getPrecision()==other.getPrecision()) ||
            !(getRadix()==other.getRadix()) ||
            !(getScale()==other.getScale()) ) {
        	return false;
        }
        
        return true;
    }
    
    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        int result = super.hashCode();

        // string properties
        if (!StringUtil.isEmpty(getDatatype())) {
            result = HashCodeUtil.hashCode(result, getDatatype());
        }
        if (!StringUtil.isEmpty(getDefaultValue())) {
            result = HashCodeUtil.hashCode(result, getDefaultValue());
        }
        if (!StringUtil.isEmpty(getDirection())) {
            result = HashCodeUtil.hashCode(result, getDirection());
        }
        if (!StringUtil.isEmpty(getNativeType())) {
            result = HashCodeUtil.hashCode(result, getNativeType());
        }
        if (!StringUtil.isEmpty(getNullable())) {
            result = HashCodeUtil.hashCode(result, getNullable());
        }

        result = HashCodeUtil.hashCode(result, getLength());
        result = HashCodeUtil.hashCode(result, getPrecision());
        result = HashCodeUtil.hashCode(result, getRadix());
        result = HashCodeUtil.hashCode(result, getScale());

        return result;
    }    

}
