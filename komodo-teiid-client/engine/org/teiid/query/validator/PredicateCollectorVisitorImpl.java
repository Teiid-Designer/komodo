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

package org.teiid.query.validator;

import java.util.ArrayList;
import java.util.Collection;

import org.komodo.spi.query.sql.PredicateCollectorVisitor;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.sql.lang.BetweenCriteriaImpl;
import org.teiid.query.sql.lang.CompareCriteriaImpl;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.lang.ExistsCriteriaImpl;
import org.teiid.query.sql.lang.IsNullCriteriaImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.MatchCriteriaImpl;
import org.teiid.query.sql.lang.PredicateCriteria;
import org.teiid.query.sql.lang.SetCriteriaImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl;
import org.teiid.query.sql.lang.SubquerySetCriteriaImpl;
import org.teiid.query.sql.navigator.PreOrderNavigator;


/**
 * <p>Walk a tree of language objects and collect any predicate criteria that are found.
 * A predicate criteria is of the following types: </p>
 *
 * <ul>
 * <li>{@link CompareCriteriaImpl} CompareCriteria</li>
 * <li>{@link MatchCriteriaImpl} MatchCriteria</li>
 * <li>{@link SetCriteriaImpl} SetCriteria</li>
 * <li>{@link SubquerySetCriteriaImpl} SubquerySetCriteria</li>
 * <li>{@link IsNullCriteriaImpl} IsNullCriteria</li>
 * </ul>
 */
public class PredicateCollectorVisitorImpl extends TCLanguageVisitorImpl
    implements PredicateCollectorVisitor<BaseLanguageObject, CriteriaImpl> {

    private Collection<CriteriaImpl> predicates;

    /**
     * Construct a new visitor with the default collection type, which is a
     * {@link java.util.ArrayList}.
     * @param teiidVersion
     */
    public PredicateCollectorVisitorImpl(TeiidVersion teiidVersion) {
        super(teiidVersion);
        this.predicates = new ArrayList<CriteriaImpl>();
    }

    /**
     * Visit a language object and collect criteria.  This method should <b>NOT</b> be
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(BetweenCriteriaImpl obj) {
        this.predicates.add(obj);
    }

    /**
     * Visit a language object and collect criteria.  This method should <b>NOT</b> be
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(CompareCriteriaImpl obj) {
        this.predicates.add(obj);
    }

    /**
     * Visit a language object and collect criteria.  This method should <b>NOT</b> be
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(IsNullCriteriaImpl obj) {
        this.predicates.add(obj);
    }

    /**
     * Visit a language object and collect criteria.  This method should <b>NOT</b> be
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(MatchCriteriaImpl obj) {
        this.predicates.add(obj);
    }

    /**
     * Visit a language object and collect criteria.  This method should <b>NOT</b> be
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(SetCriteriaImpl obj) {
        this.predicates.add(obj);
    }

    /**
     * @see TCLanguageVisitorImpl#visit(ExistsCriteriaImpl)
     */
    @Override
    public void visit(ExistsCriteriaImpl obj) {
        this.predicates.add(obj);
    }

    /**
     * @see TCLanguageVisitorImpl#visit(SubqueryCompareCriteriaImpl)
     */
    @Override
    public void visit(SubqueryCompareCriteriaImpl obj) {
        this.predicates.add(obj);
    }

	/**
	 * Visit a language object and collect criteria.  This method should <b>NOT</b> be
	 * called directly.
	 * @param obj Language object
	 */
	@Override
    public void visit(SubquerySetCriteriaImpl obj) {
		this.predicates.add(obj);
	}

    /**
     * Get a collection of predicates discovered while visiting.
     * @return Collection of {@link PredicateCriteria} subclasses.
     */
    public Collection<CriteriaImpl> getPredicates() {
        return this.predicates;
    }

    @Override
    public Collection<CriteriaImpl> findPredicates(BaseLanguageObject obj) {
        if(obj != null) {
            PreOrderNavigator.doVisit(obj, this);
        }
        return getPredicates();
    }

    /**
     * Helper to quickly get the predicates from obj
     * @param obj Language object
     * @return collection of {@link CriteriaImpl} objects
     */
    public static final Collection<CriteriaImpl> getPredicates(BaseLanguageObject obj) {
        PredicateCollectorVisitorImpl visitor = new PredicateCollectorVisitorImpl(obj.getTeiidVersion());
        return visitor.findPredicates(obj);
    }

}
