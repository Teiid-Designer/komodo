/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.komodo.spi.query;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import org.komodo.spi.annotation.AnnotationUtils;
import org.komodo.spi.runtime.version.TeiidVersion;

/**
 *
 */
public interface CriteriaOperator {

    /**
     * Operators used in criteria clauses
     */
    public enum Operator {

        /** Constant indicating the two operands are equal. */
        EQ("="), //$NON-NLS-1$
    
        /** Constant indicating the two operands are not equal. */
        NE("<>", "!="), //$NON-NLS-1$ //$NON-NLS-2$
    
        /** Constant indicating the first operand is less than the second. */
        LT("<"), //$NON-NLS-1$
    
        /** Constant indicating the first operand is greater than the second. */
        GT(">"), //$NON-NLS-1$

        /** Constant indicating the first operand is less than or equal to the second. */
        LE("<="), //$NON-NLS-1$
    
        /** Constant indicating the first operand is greater than or equal to the second. */
        GE(">="); //$NON-NLS-1$

        private Collection<String> symbols = new ArrayList<String>();

        private Operator(String... symbols) {
            this.getSymbols().addAll(Arrays.asList(symbols));
        }

        @Override
        public String toString() {
            String symbol = this.getSymbols().iterator().next();
            if (symbol == null || symbol.length() == 0)
                return "??"; //$NON-NLS-1$

            return symbol;
        }

        /**
         * @return the index of the operator
         */
        public int getIndex() {
            return ordinal() + 1;
        }

        /**
         * Operators can have more than one symbol
         * representing them
         *
         * @return collection of symbols delineating the operator
         */
        public Collection<String> getSymbols() {
            return symbols;
        }

        /**
         * @param other value to test against
         *
         * @return this operator's index is less than other's
         */
        public boolean isLessThan(Operator other) {
            return this.getIndex() < other.getIndex();
        }

        /**
         * @param other value to test against
         *
         * @return this operator's index is greater than other's
         */
        public boolean isGreaterThan(Operator other) {
            return this.getIndex() > other.getIndex();
        }

        /**
         * @param index value
         * @return {@link Operator} with the given quantifier index
         */
        public static Operator findOperator(int index) {
            for (Operator op : values()) {
                if (op.getIndex() == index)
                    return op;
            }

            throw new IllegalStateException();
        }

        /**
         * @param version of the parser
         * @param symbol value
         *
         * @return the {@link Operator} for the given string representation
         */
        public static Operator getOperator(TeiidVersion version, String symbol) {
            for (Operator operator : Operator.values()) {

                boolean foundSymbol = false;
                for (String opSymbol : operator.getSymbols()) {
                    if (opSymbol.equalsIgnoreCase(symbol)) {
                        foundSymbol = true;
                        break;
                    }
                }

                if (! foundSymbol)
                    continue;

                if (! AnnotationUtils.isApplicable(operator, version))
                    continue;

                return operator;
            }

            throw new UnsupportedOperationException("Symbol '" + symbol + "' has no operator in version " + version); //$NON-NLS-1$ //$NON-NLS-2$
        }

        /**
         * @param name value
         * @return Operator for given name
         */
        public static Operator findOperator(String name) {
            if (name == null)
                return null;

            name = name.toUpperCase();
            for (Operator operator : values()) {
                if (operator.name().equals(name))
                    return operator;
            }

            return null;
        }
    }
}
