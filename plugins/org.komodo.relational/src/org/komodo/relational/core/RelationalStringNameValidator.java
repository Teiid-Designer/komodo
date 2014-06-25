package org.komodo.relational.core;

import org.komodo.core.StringNameValidator;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.RELATIONAL;


/**
 * This class provides the RelationlStringNameRule the ability to relax the valid characters allowed for relational names.
 * 
 *
 *
 * @since 8.0
 */
public class RelationalStringNameValidator extends StringNameValidator {
	
//	public static String DESIGNER_CORE_PACKAGE = ModelerCore.class.getPackage().getName();
	@SuppressWarnings("javadoc")
	public static char[] INVALID_CHARS = {' ', '.'};
	@SuppressWarnings("javadoc")
	public static char[] INVALID_QUOTED_CHARS = {' '};
	
	boolean isTable = false;
	boolean restrictChars = false;

	/**
	 * 
	 */
	public RelationalStringNameValidator() {
		super();
//		initializePreference();
	}
	
	/**
	 * @param isTable 'true' if table is being validated
	 */
	public RelationalStringNameValidator(boolean isTable) {
		super(new char[] {UNDERSCORE_CHARACTER});
		this.isTable = isTable;
//		initializePreference();
	}
	
	/**
	 * @param isTable 'true' if table is being validated
	 * @param restrictChars 'true' to restrict chars
	 */
	public RelationalStringNameValidator(boolean isTable, boolean restrictChars) {
		this(isTable);
		this.restrictChars = restrictChars;
	}
	
//	private void initializePreference() {
//        String value = ModelerCore.getPreferences(DESIGNER_CORE_PACKAGE).get(ValidationPreferences.RELATIONAL_NAME_CHARACTER_RESTRICTION, null);
//
//        // if no value set get default value
//        if (CoreStringUtil.isEmpty(value)) {
//            value = ModelerCore.getDefaultPreferences(DESIGNER_CORE_PACKAGE).get(ValidationPreferences.RELATIONAL_NAME_CHARACTER_RESTRICTION, null);
//        }
//        if (value != null && value.equals(ValidationDescriptor.ERROR)) {
//        	this.restrictChars = true;
//        }
//	}

	@Override
	public String getValidNonLetterOrDigitMessageSuffix() {
		if( isTable ) {
			return Messages.getString(RELATIONAL.RelationalStringNameValidator_or_other_valid_table_characters);
		}
		return super.getValidNonLetterOrDigitMessageSuffix();
	}
	
	@Override
	protected String isValidChar(char c, int index) {
		if( restrictChars ) {
			return super.isValidChar(c, index);
		}
		return isValidRelationalNameCharacter(c, index, false);
	}
	
	@Override
	protected String isValidCharInDoubleQuotes(char c, int index) {
//		if( restrictChars ) {
//			return super.isValidChar(c, index);
//		}
		
		return isValidRelationalNameCharacter(c, index, true);
	}
	
	private String isValidRelationalNameCharacter(char c, int index, boolean doubleQuoted) {
		boolean valid = true;
		
		if( index == 0 ) {
			valid = Character.isLetter(c);
		} else {
			if( doubleQuoted ) {
				for( char nextChar : INVALID_QUOTED_CHARS ) {
					if( c == nextChar ) {
						valid = false;
						break;
					}
				}
			} else {
				for( char nextChar : INVALID_CHARS ) {
					if( c == nextChar ) {
						valid = false;
						break;
					}
				}
			}
		}
		
		if( !valid ) {
			final Object[] params = new Object[] {new Character(c), new Integer(index+1), getValidNonLetterOrDigitMessageSuffix()};
			return Messages.getString(RELATIONAL.RelationalStringNameValidator_nameIsInvalidTheCharacterAt, params);
		}
		
		return null;
	}
	
	/**
	 * @param restrictChars the restrictChars to set
	 */
	public void setRestrictChars(boolean restrictChars) {
		this.restrictChars = restrictChars;
	}
	
	/**
	 * Set isTable flag
	 * @param isTable the table flag
	 */
	public void setIsTable(boolean isTable) {
		this.isTable = isTable;
	}

}
