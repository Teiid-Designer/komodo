package org.komodo.spi.runtime;

/**
 * The parent teiid instance of a Teiid instance
 */
public interface ITeiidParent {

	/**
	 * @return the actual instance of the teiid parent
	 */
	Object getParentObject();

}
