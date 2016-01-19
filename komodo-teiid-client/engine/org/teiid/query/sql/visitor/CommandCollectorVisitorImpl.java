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

package org.teiid.query.sql.visitor;

import java.util.ArrayList;
import java.util.List;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.ExistsCriteriaImpl;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.SetQueryImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl;
import org.teiid.query.sql.lang.SubqueryFromClauseImpl;
import org.teiid.query.sql.lang.SubquerySetCriteriaImpl;
import org.teiid.query.sql.lang.WithQueryCommandImpl;
import org.teiid.query.sql.navigator.PreOrderNavigator;
import org.teiid.query.sql.proc.CommandStatementImpl;
import org.teiid.query.sql.proc.LoopStatementImpl;
import org.teiid.query.sql.symbol.ScalarSubqueryImpl;


/**
 * <p>This visitor class will traverse a language object tree and collect all sub-commands 
 * it finds.  It uses a List to collect the sub-commands in the order they're found.</p>
 * 
 * <p>The easiest way to use this visitor is to call the static methods which create 
 * the visitor, run the visitor, and get the collection. 
 * The public visit() methods should NOT be called directly.</p>
 */
public class CommandCollectorVisitorImpl extends TCLanguageVisitorImpl
    implements org.komodo.spi.query.sql.CommandCollectorVisitor<CommandImpl> {

    /**
     * @param teiidVersion
     */
    public CommandCollectorVisitorImpl(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    private List<CommandImpl> commands = new ArrayList<CommandImpl>();

    /**
     * Get the commands collected by the visitor.  This should best be called 
     * after the visitor has been run on the language object tree.
     * @return List of {@link org.teiid.query.sql.lang.CommandImpl}
     */
    public List<CommandImpl> getCommands() { 
        return this.commands;
    }

    /**
     * @see TCLanguageVisitorImpl#visit(org.teiid.query.sql.lang.ExistsCriteriaImpl)
     */
    @Override
    public void visit(ExistsCriteriaImpl obj) {
        this.commands.add(obj.getCommand());
    }

    @Override
    public void visit(ScalarSubqueryImpl obj) {
        this.commands.add(obj.getCommand());
    }

    @Override
    public void visit(SubqueryCompareCriteriaImpl obj) {
        this.commands.add(obj.getCommand());
    }

    /**
     * Visit a language object and collect symbols.  This method should <b>NOT</b> be 
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(SubqueryFromClauseImpl obj) {
        this.commands.add(obj.getCommand());
    }

    /**
     * Visit a language object and collect symbols.  This method should <b>NOT</b> be 
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(SubquerySetCriteriaImpl obj) {
        this.commands.add(obj.getCommand());
    }

    /**
     * Visit a language object and collect symbols.  This method should <b>NOT</b> be 
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(CommandStatementImpl obj) {
        this.commands.add(obj.getCommand());
    }    

    /**
     * Visit a language object and collect symbols.  This method should <b>NOT</b> be 
     * called directly.
     * @param obj Language object
     */
    @Override
    public void visit(LoopStatementImpl obj) {
        this.commands.add(obj.getCommand());
    }

    @Override
    public void visit(WithQueryCommandImpl obj) {
    	this.commands.add(obj.getCommand());
    }

    @Override
    public List<CommandImpl> findCommands(CommandImpl command) {
        final boolean visitCommands = command instanceof SetQueryImpl;
        PreOrderNavigator navigator = new PreOrderNavigator(this) {

            @Override
            protected void visitNode(BaseLanguageObject obj) {
                if (!visitCommands && obj instanceof CommandImpl) {
                    return;
                }
                super.visitNode(obj);
            }

        };
        command.acceptVisitor(navigator);
        return getCommands();
    }
    
    /**
     * Helper to quickly get the commands from obj
     * @param command
     * @return list of commands
     */
    public static final List<CommandImpl> getCommands(CommandImpl command) {
        CommandCollectorVisitorImpl visitor = new CommandCollectorVisitorImpl(command.getTeiidVersion());
        return visitor.findCommands(command);
    }
    
}
