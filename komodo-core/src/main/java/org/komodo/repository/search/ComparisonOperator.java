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
package org.komodo.repository.search;

/**
 *
 */
public enum ComparisonOperator {

    /**
     * Equals
     */
    EQUALS("="), //$NON-NLS-1$

    /**
     * Not Equals
     */
    NOT_EQUALS("!="), //$NON-NLS-1$

    /**
     * Less Than
     */
    LESS_THAN("<"), //$NON-NLS-1$

    /**
     * Less Than or Equal To
     */
    LESS_THAN_EQUAL_TO("<="), //$NON-NLS-1$

    /**
     * Greater Than
     */
    GREATER_THAN(">"), //$NON-NLS-1$

    /**
     * Greater Than or Equal To
     */
    GREATER_THAN_EQUAL_TO(">="), //$NON-NLS-1$

    /**
     * Like
     */
    LIKE("LIKE"), //$NON-NLS-1$

    /**
     * Not Like
     */
    NOT_LIKE("NOT LIKE"); //$NON-NLS-1$

    private String symbol;

    ComparisonOperator(String symbol) {
        this.symbol = symbol;
    }

    @Override
    public String toString() {
        return this.symbol;
    }

    /**
     * @param symbol the symbol
     * @return the operator for the symbol or null
     */
    public static ComparisonOperator findOperator(String symbol) {
        for (ComparisonOperator operator : ComparisonOperator.values()) {
            if (operator.toString().equalsIgnoreCase(symbol))
                return operator;
        }

        return null;
    }
}
