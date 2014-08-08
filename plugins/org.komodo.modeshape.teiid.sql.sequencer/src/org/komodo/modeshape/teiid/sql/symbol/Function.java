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

package org.komodo.modeshape.teiid.sql.symbol;

import java.util.Arrays;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.modeshape.teiid.sql.symbol.Function.FunctionDescriptor;
import org.komodo.spi.query.sql.symbol.IFunction;
import org.komodo.spi.udf.IFunctionDescriptor;

public class Function extends ASTNode implements Expression, IFunction<FunctionDescriptor, LanguageVisitor> {

    public static class FunctionDescriptor implements IFunctionDescriptor {

        @Override
        public String getName() {
            return null;
        }

        @Override
        public Class<?> getReturnType() {
            return null;
        }

        /**
         * @return
         */
        public Object getMethod() {
            return null;
        }
        
    }
    
    public Function(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public <T> Class<T> getType() {
        return null;
    }

    @Override
    public void setType(Class<?> type) {
    }

    @Override
    public Expression[] getArgs() {
        return null;
    }

    @Override
    public Expression getArg(int index) {
        return null;
    }

    @Override
    public FunctionDescriptor getFunctionDescriptor() {
        return null;
    }

    @Override
    public void setFunctionDescriptor(FunctionDescriptor fd) {
    }

    /**
     * @param expressions
     */
    public void setArgs(Expression[] expressions) {
    }
    @Override
    public boolean isImplicit() {
        return false;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getName() == null) ? 0 : this.getName().toUpperCase().hashCode());
        if(this.getArgs() != null && this.getArgs().length > 0 && this.getArgs()[0] != null) {
            result = prime * result + Arrays.hashCode(this.getArgs());
        }

        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        Function other = (Function)obj;

        if (this.getFunctionDescriptor() != null && other.getFunctionDescriptor() != null) {
            if (!this.getFunctionDescriptor().getMethod().equals(other.getFunctionDescriptor().getMethod())) {
                return false;
            }
        }

        if (this.getName() == null) {
            if (other.getName() != null) return false;
        } else if (!this.getName().equalsIgnoreCase(other.getName())) return false;

        if (this.isImplicit() != other.isImplicit()) return false;

        if (!Arrays.equals(this.getArgs(), other.getArgs())) return false;

        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public Function clone() {
        Function clone = new Function(this.getTeiidParser(), this.getId());

        if(getArgs() != null) {
            Expression[] cloned = new Expression[getArgs().length];
            for (int i = 0; i < getArgs().length; ++i) {
                cloned[i] = getArgs()[i].clone();
            }
            clone.setArgs(cloned);
        }
        if(getType() != null)
            clone.setType(getType());
        if(getName() != null)
            clone.setName(getName());

        return clone;
    }

}
