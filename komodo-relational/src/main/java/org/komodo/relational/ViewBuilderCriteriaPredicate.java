/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational;

import java.util.Map;
import org.komodo.utils.StringUtils;

/**
 * Contains Criteria Predicates supplied to ViewDdlBuilder
 */
public class ViewBuilderCriteriaPredicate {
    
    private static final String LH_COLUMN_KEY = "lhColName";  //$NON-NLS-1$
    private static final String RH_COLUMN_KEY = "rhColName";  //$NON-NLS-1$
    private static final String OPERATOR_KEY = "operatorName";  //$NON-NLS-1$
    private static final String COMBINE_KEYWORD_KEY = "combineKeyword";  //$NON-NLS-1$

    private static final String UNDEFINED = "undef";  //$NON-NLS-1$
    
    private String lhColumn;
    private String rhColumn;
    private String operator;
    private String combineKeyword;

    
    /**
     * Constructor
     */
    public ViewBuilderCriteriaPredicate() {
    }
    
    /**
     * Construct the Criteria predicate using the provided map
     * @param predicateMap the map of predicate values
     */
    public ViewBuilderCriteriaPredicate(Map<String,String> predicateMap) {
        setLhColumn(predicateMap.get(LH_COLUMN_KEY));
        setRhColumn(predicateMap.get(RH_COLUMN_KEY));
        setOperator(predicateMap.get(OPERATOR_KEY));
        setCombineKeyword(predicateMap.get(COMBINE_KEYWORD_KEY));
    }
    /**
     * @return the lhColumn
     */
    public String getLhColumn() {
        return StringUtils.isBlank(this.lhColumn) ? UNDEFINED : this.lhColumn;
    }
    /**
     * @param lhColumn the lhColumn to set
     */
    public void setLhColumn(String lhColumn) {
        this.lhColumn = lhColumn;
    }
    /**
     * @return the rhColumn
     */
    public String getRhColumn() {
        return StringUtils.isBlank(this.rhColumn) ? UNDEFINED : this.rhColumn;
    }
    /**
     * @param rhColumn the rhColumn to set
     */
    public void setRhColumn(String rhColumn) {
        this.rhColumn = rhColumn;
    }
    /**
     * @return the operator
     */
    public String getOperator() {
        return StringUtils.isBlank(this.operator) ? UNDEFINED : this.operator;
    }
    /**
     * @param operator the operator to set
     */
    public void setOperator(String operator) {
        this.operator = operator;
    }
    /**
     * @return the combineKeyword
     */
    public String getCombineKeyword() {
        return StringUtils.isBlank(this.combineKeyword) ? UNDEFINED : this.combineKeyword;
    }
    /**
     * @param combineKeyword the combineKeyword to set
     */
    public void setCombineKeyword(String combineKeyword) {
        this.combineKeyword = combineKeyword;
    }

    /**
     * @return 'true' if all values are provided
     */
    public boolean isComplete() {
        if(StringUtils.isBlank(this.lhColumn) || StringUtils.isBlank(this.rhColumn) || StringUtils.isBlank(this.operator) || StringUtils.isBlank(this.combineKeyword)) {
            return false;
        }
        return true;
    }
}
