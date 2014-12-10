/* Generated By:JJTree: Do not edit this line. TranslateCriteria.java Version 4.3 */
/* JavaCCOptions:MULTI=true,NODE_USES_PARSER=true,VISITOR=true,TRACK_TOKENS=false,NODE_PREFIX=,NODE_EXTENDS=,NODE_FACTORY=TeiidNodeFactory,SUPPORT_CLASS_VISIBILITY_PUBLIC=true */
package org.teiid.query.sql.lang;

import java.util.ArrayList;
import java.util.List;

import org.komodo.spi.annotation.Removed;
import org.komodo.spi.query.sql.proc.TranslateCriteria;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.parser.TeiidClientParser;

/**
 *
 */
@Removed(Version.TEIID_8_0)
public class TranslateCriteriaImpl extends CriteriaImpl implements PredicateCriteria, TranslateCriteria<TCLanguageVisitorImpl> {

    // the selector object used to determine if a type of criteria is specified 
    // on the user's query  
    private CriteriaSelectorImpl criteriaSelector;
    
    // List of comparecriteria(element-value pairs) used to translate the user's criteria
    private List<CompareCriteriaImpl> translations;

    /**
     * @param p
     * @param id
     */
    public TranslateCriteriaImpl(TeiidClientParser p, int id) {
        super(p, id);
    }

    /**
     * Get the <code>CriteriaSelector</code>
     * @return <code>CriteriaSelector</code> of this obj
     */
    public CriteriaSelectorImpl getSelector() {
        return criteriaSelector;
    }

    /**
     * Set the <code>CriteriaSelector</code>
     * @param selector The <code>CriteriaSelector</code> of this obj
     */
    public void setSelector(CriteriaSelectorImpl selector) {
        this.criteriaSelector = selector;
    }

    /**
     * Return a boolean indicating if the object has any translations.
     * @return A boolean indicating if the object has any translations
     */
    public boolean hasTranslations() {
        if(this.translations != null) {
            return (this.translations.size() > 0);          
        }
        return false;
    }

    /**
     * Set a list of comparecriteria(element-value pairs) used to translate the user's criteria.
     *
     * @param translations A list of criteria used to translate user's criteria
     */
    public void setTranslations(List<CompareCriteriaImpl> translations) {
        this.translations = translations;
    }
    
    /**
     * Add a comparecriteria(element-value pair) to the list used to translate the user's criteria.
     * @param criteria A <code>ComapareCriteria</code> object to be added to a collection
     */
    public void addTranslation(CompareCriteriaImpl criteria) {
        if(this.translations == null) {
            this.translations = new ArrayList();    
        }

        this.translations.add(criteria);
    }
    
    /**
     * Get a list of comparecriteria(element-value pairs) used to translate the user's criteria.
     * @return A list of criteria used to translate user's criteria
     */
    public List<CompareCriteriaImpl> getTranslations() {
        return this.translations;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.criteriaSelector == null) ? 0 : this.criteriaSelector.hashCode());
        result = prime * result + ((this.translations == null) ? 0 : this.translations.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        TranslateCriteriaImpl other = (TranslateCriteriaImpl)obj;
        if (this.criteriaSelector == null) {
            if (other.criteriaSelector != null) return false;
        } else if (!this.criteriaSelector.equals(other.criteriaSelector)) return false;
        if (this.translations == null) {
            if (other.translations != null) return false;
        } else if (!this.translations.equals(other.translations)) return false;
        return true;
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(TCLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public TranslateCriteriaImpl clone() {
        TranslateCriteriaImpl clone = new TranslateCriteriaImpl(this.parser, this.id);

        if(getSelector() != null)
            clone.setSelector(getSelector().clone());
        if(getTranslations() != null)
            clone.setTranslations(cloneList(getTranslations()));

        return clone;
    }

}
/* JavaCC - OriginalChecksum=f23323b45743f602279ed13dcd37babe (do not edit this line) */