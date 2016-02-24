/*
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
 */
package org.komodo.utils;

import java.util.Collection;
import java.util.Map;


/**
 * This class contains a set of static utility methods for checking method arguments. It contains many of the common checks that
 * are done, such as checking that an Object is non-null, checking the range of a value, etc. All of these methods throw
 * {@link IllegalArgumentException}.
 *
 *
 */
public class ArgCheck {

    /**
     * Check that the boolean condition is true; throw an IllegalArgumentException if not.
     *
     * @param condition The boolean condition to check
     * @param message Exception message if check fails
     * @throws IllegalArgumentException if condition is false
     */
    public static final void isTrue(boolean condition, String message) {
        if (!condition) {
            throw new IllegalArgumentException(message);
        }
    }
//
//    /**
//     * Check that the value is non-negative (>=0).
//     *
//     * @param value Value
//     * @throws IllegalArgumentException If value is negative (<0)
//     */
//    public static final void isNonNegative(int value) {
//        isNonNegative(value, null);
//    }

    /**
     * Check that the value is non-negative (>=0).
     *
     * @param value Value
     * @param message Exception message if check fails
     * @throws IllegalArgumentException If value is negative (<0)
     */
    public static final void isNonNegative(int value, String message) {
        if (value < 0) {
            final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isNonNegativeInt);
            throw new IllegalArgumentException(msg);
        }
    }
//
//    /**
//     * Check that the value is positive (>0).
//     *
//     * @param value Value
//     * @throws IllegalArgumentException If value is non-positive (<=0)
//     */
//    public static final void isPositive(int value) {
//        isPositive(value, null);
//    }
//
//    /**
//     * Check that the value is positive (>0).
//     *
//     * @param value Value
//     * @param message Exception message if check fails
//     * @throws IllegalArgumentException If value is non-positive (<=0)
//     */
//    public static final void isPositive(int value, String message) {
//        if (value <= 0) {
//            final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isPositiveInt);
//            throw new IllegalArgumentException(msg);
//        }
//    }
//
//    /**
//     * Check that the string is non-null and has length > 0
//     *
//     * @param value Value
//     * @throws IllegalArgumentException If value is null or length == 0
//     */
//    public static final void isNotZeroLength(String value) {
//        isNotZeroLength(value, null);
//    }

    /**
     * Check that the string is non-null and has length > 0
     *
     * @param value Value
     * @param message Exception message if check fails
     * @throws IllegalArgumentException If value is null or length == 0
     */
    public static final void isNotZeroLength(String value, String message) {
        isNotNull(value);
        if (value.length() <= 0) {
            final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isStringNonZeroLength);
            throw new IllegalArgumentException(msg);
        }
    }

    /**
     * Check that the object is non-null
     *
     * @param value Value
     * @throws IllegalArgumentException If value is null
     */
    public static final void isNotNull(Object value) {
        isNotNull(value, null);
    }

    /**
     * Check that the object is non-null
     *
     * @param value Value
     * @param message Exception message if check fails
     * @throws IllegalArgumentException If value is null
     */
    public static final void isNotNull(Object value, String message) {
        if (value == null) {
            final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isNonNull);
            throw new IllegalArgumentException(msg);
        }
    }

    /**
     * Check that the object is an instance of the specified Class
     *
     * @param theClass Class
     * @param value Value
     * @throws IllegalArgumentException If value is null
     */
    public static final void isInstanceOf(Class<?> theClass, Object value) {
        isInstanceOf(theClass, value, null);
    }

    /**
     * Check that the object is an instance of the specified Class
     *
     * @param theClass Class
     * @param value Value
     * @param message Exception message if check fails
     * @throws IllegalArgumentException If value is null
     */
    public static final void isInstanceOf(Class<?> theClass, Object value, String message) {
        isNotNull(value);
        if (!theClass.isInstance(value)) {
            final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isInstanceOf,
                                                                              theClass.getName(),
                                                                              value.getClass().getName());
            throw new IllegalArgumentException(msg);
        }
    }
//
//    /**
//     * Check that the collection is not empty
//     *
//     * @param collection Collection
//     * @throws IllegalArgumentException If collection is null or empty
//     */
//    public static final void isNotEmpty(Collection collection) {
//        isNotEmpty(collection, null);
//    }

    /**
     * Check that the collection is not empty.
     *
     * @param collection
     *        the collection being checked (can be <code>null</code>)
     * @param message
     *        the error message if check fails
     * @throws IllegalArgumentException
     *         if the collection is null or empty
     */
    public static final void isNotEmpty( final Collection< ? > collection,
                                         final String message ) {
        isNotNull( collection );

        if (collection.isEmpty()) {
            final String msg = message != null ? message : Messages.getString( Messages.ArgCheck.isCollectionNotEmpty );
            throw new IllegalArgumentException( msg );
        }
    }

    /**
     * Check that the map is not empty
     * @param map Map 
     * @throws IllegalArgumentException If map is null or empty
     */
    public static final void isNotEmpty(Map<?, ?> map) {
        isNotEmpty(map,null);
    }

    /**
     * Check that the map is not empty
     * @param map Map 
     * @param message Exception message if check fails
     * @throws IllegalArgumentException If map is null or empty
     */
    public static final void isNotEmpty(Map<?, ?> map, String message) {
        isNotNull(map);
        if(map.isEmpty()) {
            final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isMapNotEmpty);
            throw new IllegalArgumentException(msg);
        }
    }

    /**
     * Check that the string is not empty
     *
     * @param string String
     * @throws IllegalArgumentException If string is null or empty
     *
     */
    public static final void isNotEmpty(String string) {
        isNotZeroLength(string, null);
    }

    /**
     * Check that the string is not empty
     *
     * @param string String
     * @param message Exception message if check fails
     * @throws IllegalArgumentException If string is null or empty
     *
     */
    public static final void isNotEmpty(String string, String message) {
        isNotZeroLength(string, message);
    }

    /**
     * Check that the array is not <code>null</code> and not empty.
     *
     * @param items the array being checked
     * @param message the message if check fails
     * @throws IllegalArgumentException if array is <code>null</code> or empty
     *
     */
    public static final void isNotEmpty(final Object[] items,
                                        final String message) {
        isNotNull(items, message);

        if (items.length == 0) {
            final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isArrayNotEmpty);
            throw new IllegalArgumentException(msg);
        }
    }
//
//    /**
//     * Check that the collection contains the value
//     *
//     * @param collection Collection to check
//     * @param value Value to check for, may be null
//     * @throws IllegalArgumentException If collection is null or doesn't contain value
//     */
//    public static final void contains(Collection collection, Object value) {
//        contains(collection, value, null);
//    }
//
//    /**
//     * Check that the collection contains the value
//     *
//     * @param collection Collection to check
//     * @param value Value to check for, may be null
//     * @param message Exception message if check fails
//     * @throws IllegalArgumentException If collection is null or doesn't contain value
//     */
//    public static final void contains(Collection collection, Object value, String message) {
//        isNotNull(collection);
//        if (!collection.contains(value)) {
//            final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.contains);
//            throw new IllegalArgumentException(msg);
//        }
//    }
//
//    /**
//     * Check that two boolean values are equal
//     *
//     * @param value1 the first boolean value
//     * @param value2 the second boolean value
//     * @throws IllegalArgumentException if booleans are not equal
//     */
//    public static final void isEqual(boolean value1, boolean value2) {
//        isEqual(value1, value2, null);
//    }
//
//    /**
//     * Check that two boolean values are equal
//     *
//     * @param value1 the first boolean value
//     * @param value2 the second boolean value
//     * @param message Exception message if check fails
//     * @throws IllegalArgumentException if booleans are not equal
//     */
//    public static final void isEqual(boolean value1, boolean value2, String message) {
//        if (value1 != value2) {
//            final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isEqual, new Object[] {
//                new Boolean(value1), new Boolean(value2)});
//            throw new IllegalArgumentException(msg);
//        }
//    }
//
//    /**
//     * Checks if two booleans are NOT equal
//     *
//     * @param value1 the first boolean value
//     * @param value2 the second boolean value
//     * @throws IllegalArgumentException if booleans are equal
//     */
//    public static final void isNotEqual(boolean value1, boolean value2) {
//        isNotEqual(value1, value2, null);
//    }
//
//    /**
//     * Checks if two booleans are NOT equal
//     *
//     * @param value1 the first boolean value
//     * @param value2 the second boolean value
//     * @param message Exception message if check fails
//     * @throws IllegalArgumentException if booleans are equal
//     */
//    public static final void isNotEqual(boolean value1, boolean value2, String message) {
//        if (value1 == value2) {
//            final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isNotEqual, new Object[] {
//                new Boolean(value1), new Boolean(value2)});
//            throw new IllegalArgumentException(msg);
//        }
//    }
//
//    /**
//     * Checks if two integer values are equal
//     *
//     * @param value1 the first integer value
//     * @param value2 the second integer value
//     * @throws IllegalArgumentException if booleans are equal
//     */
//    public static final void isEqual(int value1, int value2) {
//        isEqual(value1, value2, null);
//    }
//
//    /**
//     * Checks if two integer values are equal
//     *
//     * @param value1 the first integer value
//     * @param value2 the second integer value
//     * @param message Exception message if check fails
//     * @throws IllegalArgumentException if booleans are equal
//     */
//    public static final void isEqual(int value1, int value2, String message) {
//        if (value1 != value2) {
//            final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isEqual, new Object[] {
//                new Integer(value1), new Integer(value2)});
//            throw new IllegalArgumentException(msg);
//        }
//    }
//
//    /**
//     * Checks if two integer values are NOT equal
//     *
//     * @param value1 the first integer value
//     * @param value2 the second integer value
//     * @throws IllegalArgumentException if booleans are equal
//     */
//    public static final void isNotEqual(int value1, int value2) {
//        isNotEqual(value1, value2, null);
//    }
//
//    /**
//     * Checks if two integer values are NOT equal
//     *
//     * @param value1 the first integer value
//     * @param value2 the second integer value
//     * @param message Exception message if check fails
//     * @throws IllegalArgumentException if booleans are equal
//     */
//    public static final void isNotEqual(int value1, int value2, String message) {
//        if (value1 == value2) {
//            final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isNotEqual, new Object[] {
//                new Integer(value1), new Integer(value2)});
//            throw new IllegalArgumentException(msg);
//        }
//    }
//
//    /**
//     * Compares with object1.equals(object2).
//     *
//     * @param object1 the first object
//     * @param object2 the second object
//     * @throws IllegalArgumentException if booleans are equal
//     */
//    public static final void isEqual(Object object1, Object object2) {
//        isEqual(object1, object2, null);
//    }
//
//    /**
//     * Compares with object1.equals(object2).
//     *
//     * @param object1 the first object
//     * @param object2 the second object
//     * @param message Exception message if check fails
//     * @throws IllegalArgumentException if booleans are equal
//     */
//    public static final void isEqual(Object object1, Object object2, String message) {
//        if (object1 == null) {
//            if (object2 != null) {
//                final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isEqual, new Object[] {
//                    object1, object2});
//                throw new IllegalArgumentException(msg);
//            }
//            // else both are null
//        } else {
//            if (object2 == null) {
//                final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isEqual, new Object[] {
//                    object1, object2});
//                throw new IllegalArgumentException(msg);
//            }
//            // else both are not null
//            if (!object1.equals(object2)) {
//                final String msg = message != null ? message : Messages.getString(Messages.ArgCheck.isEqual, new Object[] {
//                    object1, object2});
//                throw new IllegalArgumentException(msg);
//            }
//        }
//    }
//

    /**
     * @param arg
     *        the argument being checked (cannot be empty)
     * @param illegalCharacters
     *        the characters the arg cannot contain (can be empty)
     * @param message
     *        the error message used when an illegal character is found (can be empty if a generic message should be used)
     */
    public static void doesNotContain( final String arg,
                                       final String illegalCharacters,
                                       final String message ) {
        isNotEmpty( arg, message );

        if (!StringUtils.isBlank( illegalCharacters )) {
            for (final char c : illegalCharacters.toCharArray()) {
                if (arg.indexOf( c ) != -1) {
                    String msg = message;

                    if (StringUtils.isBlank( message )) {
                        msg = "Argument \"" + arg + "\" contains the illegal character \"c\""; //$NON-NLS-1$ //$NON-NLS-2$
                    }

                    throw new IllegalArgumentException( msg );
                }
            }
        }
    }

    /**
     * Can't construct - utility class
     */
    private ArgCheck() {
    }

}
