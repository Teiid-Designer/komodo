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
package org.komodo.modeshape.teiid.parser.bnf.clause;

import java.util.ArrayList;
import java.util.List;

/**
 *
 */
public class CaseStatement {

    private List<String> declarations = new ArrayList<String>();

    private List<String> statements = new ArrayList<String>();

    /**
     * @return the declarations
     */
    public List<String> getDeclarations() {
        return this.declarations;
    }

    /**
     * @param declaration
     */
    public void addDeclaration(String declaration) {
        this.declarations.add(declaration);
    }

    /**
     * @return the statements
     */
    public List<String> getStatements() {
        return this.statements;
    }

    /**
     * @param statement
     */
    public void addStatement(String statement) {
        statements.add(statement);
    }

    /**
     * @return the case statement is for switch rather than if-else
     */
    public boolean isSwitch() {
        for (String declaration : getDeclarations()) {
            if (declaration.matches("[\\s]*case [a-zA-Z_0-9]+:.*")) //$NON-NLS-1$
                return true;
        }

        return false;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.declarations == null) ? 0 : this.declarations.hashCode());
        result = prime * result + ((this.statements == null) ? 0 : this.statements.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        CaseStatement other = (CaseStatement)obj;
        if (this.declarations == null) {
            if (other.declarations != null)
                return false;
        } else if (!this.declarations.equals(other.declarations))
            return false;
        if (this.statements == null) {
            if (other.statements != null)
                return false;
        } else if (!this.statements.equals(other.statements))
            return false;
        return true;
    }
}
